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
  return false; // set to true for development of code below, false will use the older per cell code
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
    if (top[1] < arr[i][0]) {
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
 * Implementation in progress
 */
RemoteFileReader.prototype.readMultiRange = function(rangeList, callback) {
  // There are limitations in the size of the Range request header
  // The the maximum length is between 3812 and 3831 characters long
  // For this reason it is beneficial to merge adjacent requests
  // Beyond that the only option is to perform multiple requests

  // Merge adjacent ranges
  var rangesMerged = this.mergeRanges(rangeList);
  
  // Now these ranges continue to be too many for a single request
  
  debugger;
  
  var bytesArg = "bytes=";
  var isFirst = true;
  for (var i = 0; i < rangeList.length; i++) {
    if(!isFirst) {bytesArg = bytesArg.concat(', ')};
    bytesArg = bytesArg.concat(rangeList[i].start, '-', rangeList[i].end-1);
    isFirst=false;
  }
  
  var xhr = new XMLHttpRequest;
  xhr.onreadystatechange = function(evt) {
    if (xhr.readyState == XMLHttpRequest.DONE) {
      var httpStatus = evt.target.status;
      if (httpStatus == 206) {
        // Partial content returned as requested
      } else if(httpStatus == 200) {
        console.error('Complete file returned by server!')
      } else if(httpStatus == 400) {
        
      } else if(httpStatus == 416) {
       // Requested Range Not Satisfiable 
      } else {
        console.error('Unknown http Status')
      }
      // Let's see what we got!
      console.log(evt.target);
      debugger;
      
    }
  }
  
  xhr.open('GET', this.url, true);
  xhr.setRequestHeader('Range', bytesArg);
  xhr.responseType = "arraybuffer";
  xhr.send(null);
}

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

