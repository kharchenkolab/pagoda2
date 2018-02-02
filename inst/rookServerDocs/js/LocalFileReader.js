"use strict";

/**
  * Allows local file reading by location
* @descptiont allows local file reading
* @todo consider implementing queue system to prevent multiple requests form aborting
* @constructor
* @opt_file File object to read
*/
function LocalFileReader(opt_file) {
  this.file = opt_file;
  if (! this.checkBrowserSupport() ) {
    throw new Error('Browser does not support local file operations');
    return null;
  }

}

/**
 * Reports if multi regions requests are supported by this file reader
 * Always returns false
 */
LocalFileReader.prototype.supportsMultiRequest = function() {
  return false;
}

/**
  * Read the specified block number
* @param File object to read from
* @param blockSize size of each block
* @param blockNumber which block to read 0-indexed
*/
  LocalFileReader.prototype.readBlock = function(blockSize, blockNumber, callback) {
    var start = blockSize * blockNumber;
    var end = blockSize * (blockNumber + 1);
    this.readRange(start, end, callback);
  }

/**
  * Read specified part of file
  * @description read specific part of a file as specified by start and end and return
  * ArrayBuffer with the data
  * @start beginning of the data 0-indexed
  * @end end of the data 0-indexed, but not returned (effectively 1-indexed)
  * @callback a function to call when the data is ready
  */
LocalFileReader.prototype.readRange = function(start, end, callback) {
  var reader = new FileReader();
  reader.onloadend = function(evt) {
    if(evt.target.readyState == FileReader.DONE) {
      callback(evt.target.result);
    }
  }
  var blob = this.file.slice(start,end);
  reader.readAsArrayBuffer(blob);
}

LocalFileReader.prototype.readRangeAsText = function(start, end, callback) {
  var reader = new FileReader();
  reader.onloadend = function(evt) {
    if(evt.target.readyState == FileReader.DONE) {
      callback(evt.target.result);
    }
  }
  var blob = this.file.slice(start,end);
  reader.readAsText(blob);
}

/**
 * Check if the browser supports access to files via the FileReader
 */
LocalFileReader.prototype.checkBrowserSupport = function() {
  if (window.File && window.FileReader && window.FileList && window.Blob ) {
    return true;
  } else {
    return false;
  }
}

