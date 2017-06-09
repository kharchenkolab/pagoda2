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


//////////////////////////////////////////////
// p2FormatReader
//////////////////////////////////////////////



/**
 * Low-level reader from the pagoda file format
 * @description responsible for low-level variable size block reading
 * operations from the pagoda2 file format
 * @parameters fileReader a p2FileReader object to dispatch requests to
 * @constructor
 */
function p2FormatReader(opt_FileReader) {
    this.filereader = opt_FileReader; // Keep track of the file reader provided

    this.headerBufffer = null; // ArrayBuffer holding the raw data

    this.indexBuffer = null; // ArrayBuffer holding the raw data
    this.indexOffset = null; // Start position of the index in the file

    this.indexSize = null; // Size of the index in bytes
    this.index = null; // Decoded index as hash

    this.blockSize = null; // Blocksize for this file, set by the header (currently 2MB)

    this.dataOffset = null; // Where the data starts

    //this.readHeaderIndex(); // Load the file header and index


    this.INITIALIZING = 0;
    this.READY = 1;

    this.state = this.INITIALIZING;

    // Hash array of listeners
    this.listeners = {};

}

p2FormatReader.prototype.dispatchEvent = function(eventName) {
  if (Array.isArray(this.listeners[eventName])) {
      var f = this.listeners[eventName].pop();
      while(typeof f !== 'undefined') {
        if (typeof f === 'function') {
          f();
        } else {
          console.error('Non function event listener found');
        }
        f = this.listeners[eventName].pop();
      }
  }
}

p2FormatReader.prototype.addEventListener = function(eventName, fn) {
  if (!Array.isArray(this.listeners[eventName])) {
    // First listener for this event
    this.listeners[eventName] = [];
  }

  this.listeners[eventName].push(fn);
}

p2FormatReader.prototype.removeEventListener = function(eventName, fn) {
  // TODO
}

/**
 * Load the file header and index
 */
p2FormatReader.prototype.readHeaderIndex = function() {
    var context = this;

    // In principle this does not need to be hardcoded here as it
    // Is provided at a fixed position in the beginning fo the header
    const FILE_HEADER_SIZE = 48;

    this.filereader.readRange(0, FILE_HEADER_SIZE, function(data) {
	context.headerBuffer = data;

	// Check indentifier
	// First 32 bytes must be 'pagoda2datafile' followed by zeros
	// Here we will just check that the initial string matches
	// and ignore the 0s
	var fileIdentifierArray = new Uint8Array(data.slice(0,15));
	var fileIdentifierString = String.fromCharCode.apply(null, fileIdentifierArray);
	if (fileIdentifierString != 'pagoda2datafile') {
	    throw new Error('File is not a pagoda2 data file');
	}

	// Check File version
	var majorVersion = new Uint8Array(data.slice(32,33));
	var minorVersion = new Uint8Array(data.slice(33,34));
	if (!(majorVersion == 1 && minorVersion ==0)) {
	    throw new Error('Incompatible file version');
	}

	// Read Flags -- Not currently used
	var flags = new Uint8Array(data.slice(34,36));

	// Block size in bytes
	var blockSize = new Uint32Array(data.slice(36,40));
	context.blockSize =  blockSize[0];

	// Read header size -- context is currently fixed
	var headerSize = new Uint32Array(data.slice(40,44));
	context.indexOffset = headerSize[0];

	// Read index size
	var indexSizeArray = new Uint32Array(data.slice(44,48));
	context.indexSize = indexSizeArray[0];

	// Calculate the data start position
	context.dataOffset = FILE_HEADER_SIZE + context.indexSize;

	// Load the index
	if (context.indexOffset === null || context.indexSize == null) {
	    throw new Error('Attempt to read index when the file header is not loaded');
	}

	var indexStart = context.indexOffset;
	var indexEnd = context.indexOffset + context.indexSize + 1;

	// Request index data
	context.filereader.readRange(context.indexOffset, indexEnd, function(data) {

	    // Initialise an empty index
	    context.index = {};

	    // Number of index entries
	    const INDEX_ENTRY_SIZE = 140; // in bytes
	    var nIndexEntries = (context.indexSize) / INDEX_ENTRY_SIZE;

	    if (nIndexEntries - Math.round(nIndexEntries) > 1e-16) {
    	  throw new Error('Internal Error invalid number of index entries');
    	 }


    	 for (var i = 0; i < nIndexEntries; i++) {
    		// Offset within the index data
    		var indexEntryOffset = i * INDEX_ENTRY_SIZE;

    		const KEY_OFFSET = 0;
    		const KEY_SIZE = 128;

    		var keyUint8Array = new Uint8Array(data.slice(KEY_OFFSET +  indexEntryOffset,
    		            KEY_OFFSET + KEY_SIZE + indexEntryOffset));

        // Find length of null temrminated string
    		var keyLength;
    		for (keyLength = 0; keyLength < KEY_SIZE; keyLength++) {
    		    var c = new Uint8Array(keyUint8Array.slice(keyLength, keyLength + 1));
    		    if (c[0] === 0) {
    		      // Found zero
    			    break;
    		    }
    		}


    		////

    		// Get just the key entry
    		var c_key = String.fromCharCode.apply(null, keyUint8Array.slice(0,keyLength));


    		// Read the size in  blocks
    		const OFFSET_SIZE_BLOCKS = 128;
    		const SIZE_SIZE_BLOCKS = 4;
    		var offsetUint32Array = new Uint32Array(data.slice(OFFSET_SIZE_BLOCKS + indexEntryOffset,
    								   OFFSET_SIZE_BLOCKS + SIZE_SIZE_BLOCKS + indexEntryOffset));
    		var c_size = offsetUint32Array[0];

    		// Read the offset
    		const OFFSET_OFFSET = 132; // offset of the offset field within the index entry
    		const OFFSET_SIZE = 4; // Uint32
    		var offsetUint32Array = new Uint32Array(data.slice(OFFSET_OFFSET + indexEntryOffset,
    								   OFFSET_OFFSET + OFFSET_SIZE + indexEntryOffset));
    		var c_offset = offsetUint32Array[0];

    		// TODO: Read the flags -- not currenly used
    		//const OFFSET_FLAGS ...


    		// Add the index entry, offset is in blocks
    		context.index[c_key] = {'key': c_key, 'offset': c_offset, 'size': c_size};
	    }

	    context.state = context.READY; // Mark object as ready
	    if (typeof context.onReady === 'function') { context.onReady(context) };

      // Fire onready event
	    context.dispatchEvent('onready');

	    // DEBUG
	    console.log('Index structure: ', context.index);
	});
    });

}

/**
 * Retrieves a whole entry
 * @description retrieve a whole entry from the reader. This is not recommended
 * for large blocksizes
 * @entryKey the key for this entry
 */
p2FormatReader.prototype.getEntry = function(entryKey, callback, context) {
    if (typeof context === 'undefined') { context = this; }

    var entryIndexInfo = context.index[entryKey];
    var start = context.dataOffset + entryIndexInfo.offset * context.blockSize;
    var end = context.dataOffset + (entryIndexInfo.offset + entryIndexInfo.size) * context.blockSize;

    context.filereader.readRange(start,end, function(data) {
	    callback(data);
    });

}


/**
 *  Read a file entry as text data
 */
p2FormatReader.prototype.getEntryAsText = function(entryKey, callback, context) {
    if (typeof context === 'undefined') { context = this; }

    var entryIndexInfo = context.index[entryKey];
    var start = context.dataOffset + entryIndexInfo.offset * context.blockSize;
    var end = context.dataOffset + (entryIndexInfo.offset + entryIndexInfo.size) * context.blockSize;

    context.filereader.readRangeAsText(start,end, function(data) {
	    callback(data);
    });

}

/**
 * Get the specified byte range from the indicated variable size block
 */
p2FormatReader.prototype.getBytesInEntryAsText = function(entryKey, start, end, callback, context) {
    // TODO
    if (typeof context === 'undefined') {context = this;}

    var entryIndexInfo = context.index[entryKey];
    var start = context.dataOffset + entryIndexInfo.offset * context.blockSize + start;
    var end = start + end;

    context.filereader.readRangeAsText(start,end, function(data) {
	    callback(data);
    });
}


/**
 * Get the specified byte range from the indicated variable size block
 */
p2FormatReader.prototype.getBytesInEntry = function(entryKey, start, end, callback, context) {
    // TODO
    if (typeof context === 'undefined') {context = this;}

    var entryIndexInfo = context.index[entryKey];
    var start = context.dataOffset + entryIndexInfo.offset * context.blockSize + start;
    var end = start + end;

    context.filereader.readRange(start,end, function(data) {
	    callback(data);
    });
}
