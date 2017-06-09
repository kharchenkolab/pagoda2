"use strict";

/**
 * Pagoda2 file local or remote reader
 * @description Provides the abstration layer to allow transparently reading
 * arbitrary ranges from local or remote files
 * @argument opt_Type type of reader: 'local' or 'remote'
 * @argument opt_Url URL of the file to read for remote requests, null otherwise
 * @argument opt_File File object to read for local requests, null otherwise
 * @constructor
 */
function p2FileReader(opt_Type, opt_Url, opt_File) {
    this.internalReader  = null;

    if (opt_Type === 'local') {
    	if (typeof opt_File === 'undefined') {
    	    throw new Error('p2FileReader type is local and file is undefined');
    	} else {
    	    this.internalReader = new LocalFileReader(opt_File);
    	}
    } else if ( opt_Type === 'remote' ) {
    	if (typeof url === 'undefined' || url === '') {
    	    this.internalReader = new RemoteFileReader(opt_Url);
    	}
    } else {
	    throw new Error('Unknown p2FileReader type: ', opt_Type);
    }
}

/**
 * Read range of specified file
 * @description transparently reads local or remote files
 * @start start position, 0-indexed
 * @end end position, 0-indexed but not read
 * @callback the callback function
 */
p2FileReader.prototype.readRange = function(start, end, callback) {
    this.internalReader.readRange(start,end, callback);
}


p2FileReader.prototype.readRangeAsText = function(start, end, callback) {
    this.internalReader.readRangeAsText(start,end, callback);
}

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
    } else {

    }
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

