"use strict";

/**
* Pagoda2 remote file reader
* @description implements remote file reading
* @param opt_url the URL of the resource to read
* @constructor
*/
function RemoteFileReader(opt_url) {
  // TODO: Check Browser Suport
  this.url = opt_url;
}

/**
 * Reports if multi regions requests are supported by this file reader
 * Always returns true
 */
RemoteFileReader.prototype.supportsMultiRequest = function() {
  return true; // set to true for development of code below, false will use the older per cell code
}


/** 
 * Given a set of ranges in a format of array of array of start end positions
 * Merge any adjacent ranges into one and return the new ranges
 */
RemoteFileReader.prototype.mergeRanges = function(ranges) {
  //Sort by starting position
  var compareIntervals = function(a,b){
    if(a[0] < b[0]) { return -1; }
    if(a[0] > b[0]) { return 1;  }
    return 0;
  };
  var arr = ranges.sort(compareIntervals);
  // A stack
  var s = [];
  s.push(arr[0]);
  // Find adjacent regions
  var n = arr.length;
  for (var i = 1; i < n; i++) {
    var top = s[s.length - 1]; // top element
    // If no overlap push to stack
    if (top[1] + 1 < arr[i][0]) {
      s.push(arr[i]);
    } else if (top[1] < arr[i][1]) {
      top[1] = arr[i][1];
      s.pop();
      s.push(top);
    }
  }
  return s;
};

/**
 * Given a range and an array of ranges
 * find where the range comes from
 */
RemoteFileReader.prototype.findRangeInMergedRanges = function(range, mergedRanges) {
  var queryStart = range[0];
  var queryEnd = range[1];
  
  var containerRange = null;
  var containerRangeOffset = null;
  
  for (var i = 0; i < mergedRanges.length; i++){
    var s = mergedRanges[i][0]; // start
    var e = mergedRanges[i][1]; // end
    if( queryStart >= s && queryEnd <= e ) {
      containerRange = i;
      containerRangeOffset = queryStart - s;
      break;
    }
  };
  
  return {
    containerRange: containerRange, 
    containerRangeOffset: containerRangeOffset,
    rangeLength: queryEnd - queryStart + 1
  };
};

/**
 * Implementation in progress
 * @param rangeList a list of ranges
 * @param callback function to call when complete
 */
RemoteFileReader.prototype.readMultiRange = function(rangeList, callback) {
  var rfr = this;
  
  // There are limitations in the size of the Range request header
  // The the maximum length is between 3812 and 3831 characters long
  // For this reason it is beneficial to merge adjacent requests
  // Beyond that the only option is to perform multiple requests

  // Merge adjacent ranges
  //var rangesMerged = this.mergeRanges(rangeList);
  var rangesMerged = rangeList;
  
  // Split the ranges into multiple request requesting no more than nRanges 
  // ranges per request
  // FIXME: The code below breaks if we put more than 1 range in a request
  // defeating the purpose of multirange requests
  var nRanges = 1; 

  // Array of arrays, each sub array holds ranges for the corresponding request
  var requestRanges = [];
  
  // Array to keep track of the status of requests
  // 0 -- Not complete
  // 1 -- Complete
  var requestStatus = [];
  
  // Array of resulting data
  var requestData = [];
  
  var rangesMergedLength = rangesMerged.length;
  var kMax = Math.ceil(rangesMergedLength/nRanges)
  for (var k = 0; k < kMax; k++ ) {
    var startRangeIndex = k * (nRanges);
    var endRangeIndex = (k + 1) * nRanges;
    
    Math.min(((k + 1) * nRanges) - 1,rangesMergedLength);
    requestRanges[k] = rangesMerged.slice(startRangeIndex,endRangeIndex)
    // Array for request status
    requestStatus[k] = 0;
    // Array for returned data
    requestData[k] = null;
  }
  
  // Return true if all requests are completed, false otherwise
  var isComplete = function() {
    for (var i = 0; i < requestStatus.length; i++) {
      if (requestStatus[i] == 0) return false;
    }
    return true;
  };
  
  // Check if we have all the data and return
  var checkComplete = function() {
    if(isComplete()) {
      var returnData = [];
      
      // For each original range
      for (var l = 0; l < rangeList.length; l++) {
        // This is the position of the original requested range in the merged ranges
        // We get the position of the relevant range in the rangesMergedArray,
        // the offset of that data in the range and the length of the data
        // This can be optimised and calcualated for all ranges in one go.
        var posMergedRanges = rfr.findRangeInMergedRanges(rangeList[l], rangesMerged);
        
        // Now we need to find in which http request the merged range: 
        // rangesMerge[posMergedRanges.containerRangeOffset]
        // was in. We can actually calculate that, we don't need to search
        var mergedrangeRequest = Math.floor(posMergedRanges.containerRange / (nRanges));
        var mergedRangeRequestPosition = posMergedRanges.containerRange % (nRanges);
        
        // Now we need to find the offset of the merged range in the request
        // This will be the end of the last request (sum all of all previous lengths -1)
        // plus 1
        var mergedRangeOffsetInRequest = 0;
        for (var e = 0; e < mergedRangeRequestPosition; e++){
          var cr = requestRanges[mergedrangeRequest][e];
          mergedRangeOffsetInRequest += (cr[1] - cr[0]);
        } 
        
        // Finally extract the data from the request
        //if (posMergedRanges.containerRangeOffset != 0) {console.log('Error!')}
        var dataStartInRequest = mergedRangeOffsetInRequest + posMergedRanges.containerRangeOffset;
        var dataEndInRequest = dataStartInRequest + posMergedRanges.rangeLength - 1;
        returnData[l] = requestData[mergedrangeRequest].slice(dataStartInRequest, dataEndInRequest);
      }
      
      // We now have an array of array buffers that matches the original ranges in rangeList 
      callback(returnData);
    }
  };
  
  var dispatchRequest = function(rangeList, requestid) {
      // Construct bytes argument
      var bytesArg = "bytes=";
      var isFirst = true;
      for (var i = 0; i < rangeList.length; i++) {
        if(!isFirst) {bytesArg = bytesArg.concat(', ')};
        bytesArg = bytesArg.concat(rangeList[i][0], '-', rangeList[i][1]-1);
        isFirst=false;
      }
      
      // Make the request
      var xhr = new XMLHttpRequest();
      xhr.onreadystatechange = function(evt) {
        if (xhr.readyState == XMLHttpRequest.DONE) {
          var httpStatus = evt.target.status;
          console.log("Req ", requestid, ' complete. status: ', httpStatus)
          if (httpStatus == "206") {
            requestStatus[requestid] = 1;
            requestData[requestid] = evt.target.response;
            checkComplete();
          } else {
            // Problem 
            // TODO: handle this esp 406 and 200
            console.error('An error occurred processing multi partial http request!')
          }
        }
      }
      
      // Dispatch request
      xhr.open('GET', rfr.url, true);
      xhr.setRequestHeader('Range', bytesArg);
      xhr.responseType = "arraybuffer";
      xhr.send(null);      
  }

  // Dispatch all requests
  for (var u = 0; u < requestRanges.length; u++) {
    dispatchRequest(requestRanges[u], u);
  }
}  // readMultiRange()

/**
  * Read range of specified file
* @description transparently reads local or remote files
* @start start position, 0-indexed
* @end end position, 0-indexed but not read
* @callback the callback function
*/
  RemoteFileReader.prototype.readRange = function(start, end, callback) {
    var xhr = new XMLHttpRequest;
    xhr.onreadystatechange = function(evt) {
      if (xhr.readyState == XMLHttpRequest.DONE) {
        callback(evt.target.response);
      }
    }

    xhr.open('GET', this.url, true);
    var bytesArg = "bytes=".concat(start,'-',end-1);
    xhr.setRequestHeader('Range', bytesArg);
    xhr.responseType = "arraybuffer";
    xhr.send(null);
  }

  RemoteFileReader.prototype.readRangeAsText = function(start, end, callback) {
    var xhr = new XMLHttpRequest;
    xhr.onreadystatechange = function(evt) {
      if (xhr.readyState == XMLHttpRequest.DONE) {
        callback(evt.target.response);
      }
    }

    xhr.open('GET', this.url, true);
    var bytesArg = "bytes=".concat(start,'-',end-1);
    xhr.setRequestHeader('Range', bytesArg);
    xhr.responseType = "text";
    xhr.send(null);
  }

