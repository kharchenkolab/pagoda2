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
  return true; 
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
RemoteFileReader.prototype.readMultiRange = function(rangeList, callback, progressCallback) {
  var rfr = this;
  
  // Merge adjacent ranges
  var rangesMerged;
  
  // TODO: Get this to work
  var merge = false; 
  if (merge) {
    rangesMerged = this.mergeRanges(rangeList);
  } else {
    rangesMerged = rangeList;
  }
  
  // Number of ranges per request
  var nRangesPerRequest = 20;
  
  // Array of arrays, each sub array holds ranges for the corresponding request
  var requestRanges = [];
  
  // Array to keep track of the status of requests
  // 0 -- Not complete
  // 1 -- Complete
  var requestStatus = [];
  
  // Array of resulting data
  var requestData = [];
  
  var rangesMergedLength = rangesMerged.length;
  var kMax = Math.ceil(rangesMergedLength/nRangesPerRequest)
  for (var k = 0; k < kMax; k++ ) {
    var startRangeIndex = k * (nRangesPerRequest);
    var endRangeIndex = (k + 1) * nRangesPerRequest;
    
    Math.min(((k + 1) * nRangesPerRequest) - 1,rangesMergedLength);
    requestRanges[k] = rangesMerged.slice(startRangeIndex,endRangeIndex)
    // Array for request status
    requestStatus[k] = 0;
    // Array for returned data
    requestData[k] = null;
  }
  
  // Return true if all requests are completed, false otherwise
  var isComplete = function() {
    var completeCount = 0;
    const reqStatusLength = requestStatus.length
    for (var i = 0; i < reqStatusLength; i++) {
      if (requestStatus[i] === 1) 
        completeCount++
    }
    
    //console.log(completeCount/reqStatusLength);
    
    if(completeCount === reqStatusLength) {
      return true;
    } else {
      if (typeof progressCallback === 'function') {
        progressCallback(completeCount/reqStatusLength);
      } 
      return false;      
    }
  };
  
  // Check if we have all the data and return
  var checkComplete = function() {
    if(isComplete()) {
      var returnData = [];
      
      // For each original range
      for (var l = 0; l < rangeList.length; l++) {
        // This is the position of the original requested range in the merged ranges
        var posMergedRanges = rfr.findRangeInMergedRanges(rangeList[l], rangesMerged);
        // Now we need to find in which http request the merged range request
        var mergedrangeRequest = Math.floor(posMergedRanges.containerRange / (nRangesPerRequest));
        var mergedRangeRequestPosition = posMergedRanges.containerRange % (nRangesPerRequest);
        returnData[l] = requestData[mergedrangeRequest][mergedRangeRequestPosition];
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
      
      // Convert binary string to array buffer
      var _binaryToArrayBuffer = function(binary_string) {
        var len = binary_string.length;
        var bytes = new Uint8Array( len );
        for (var i = 0; i < len; i++)        {
            bytes[i] = binary_string.charCodeAt(i);
        }
        return bytes.buffer;
      }
      
      var ab2str = function(buf) {
        // Apply causes stack issues with long strings
        var a = new Uint8Array(buf);
        var b = [];
        for (var i = 0; i < a.length; i++) {
          b[i] = String.fromCharCode(a[i]);
        }
        return b.join("")
      }
      
      var processMultipart = function(response, boundary) {
        var responseText = ab2str(response);
        var responseSplit = responseText.split(`--${boundary}`);
        var data = [];
        // Ignore 1st and last entry
        for (var i = 1; i < responseSplit.length - 1; i++ ){
          var entry = responseSplit[i];
          entry = entry.substr(2,entry.length-4); // remove leading and training new lines
          // Note if the /r/n/r/n seq exists int he bninary data there is a problem
          var [ head, body ] = entry.split(/\r\n\r\n/g);
          data[i-1] = _binaryToArrayBuffer(body);
        }
        return data; // an array of buffers
      }
      
      // Make the request
      var xhr = new XMLHttpRequest();
      xhr.onreadystatechange = function(evt) {
        if (xhr.readyState == XMLHttpRequest.DONE) {
          var httpStatus = evt.target.status;
          if (httpStatus == "206") {
            // Get the multipart field separator
            var boundaryArray = xhr.getResponseHeader('Content-Type').match(/boundary=(.+)$/i);
            if (boundaryArray !== null) {
              // Its a response in chunks
              requestData[requestid] = processMultipart(xhr.response,boundaryArray[1]);
            } else {
                // Came back as a single request -- dont' need to tdo anything
                requestData[requestid] = [xhr.response];
            }
            
            requestStatus[requestid] = 1;
            checkComplete();  

          } else {
            // Problem 
            // TODO: handle this esp 406 and 200
            console.error('An error occurred processing multi partial http request!')
          }
        }
      }

      if(window.chrome) {
	  // Chrome has a problem with caching, disable it by adding random numbers here
	  var urlRequest = rfr.url.concat('?',Math.floor(Math.random()*1e6));
      } else {
	  var urlRequest = rfr.url;
      }

      
      // Dispatch request
      xhr.open('GET', urlRequest, true); // TODO: Try POST for longer ranger requests
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


      if(window.chrome) {
	  // Chrome has a problem with caching, disable it by adding random numbers here
	  var urlRequest = this.url.concat('?',Math.floor(Math.random()*1e6));
      } else {
	  var urlRequest = this.url;
      }
      
    xhr.open('GET', urlRequest, true);
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

      if(window.chrome) {
	  // Chrome has a problem with caching, disable it by adding random numbers here
	  var urlRequest = this.url.concat('?',Math.floor(Math.random()*1e6));
      } else {
	  var urlRequest = this.url;
      }
      
    xhr.open('GET', urlRequest, true);
    var bytesArg = "bytes=".concat(start,'-',end-1);
    xhr.setRequestHeader('Range', bytesArg);
    xhr.responseType = "text";
    xhr.send(null);
  }

