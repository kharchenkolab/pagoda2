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

