"use strict";

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
};

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
	  var msgBox = Ext.Msg.show({
	    title:"Error: Invalid File",
	    message: "<b>Error: invalid file provided.</b><br/> You are trying to open a file that is not a pagoda2 binary file. You either accidentally selected to wrong file or there is something wrong with the link you have.",
	    closable: false,
	    width: 200
	    //buttons: [{}]
	  });
	  //msgBox.show();

	 //   Ext.Msg.alert('Error','Error: The specified static is not a pagoda2 data file', buttons );

	    var error = new Error('File is not a pagoda2 data file');
	    throw error;

	}

	// Check File version
	var majorVersion = new Uint8Array(data.slice(32,33));
	var minorVersion = new Uint8Array(data.slice(33,34));

	if (!(majorVersion[0] == 1 && minorVersion[0] ==0)) {
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

    if (typeof entryIndexInfo !== 'undefined') {
      var start = context.dataOffset + entryIndexInfo.offset * context.blockSize;
      var end = context.dataOffset + (entryIndexInfo.offset + entryIndexInfo.size) * context.blockSize;

      context.filereader.readRangeAsText(start,end, function(data) {
  	    callback(data);
      });
    } else {
      throw new RuntimeException(STATIC_FILE_FIELD_MISSING, 'Unknown index: '.concat(entryKey))
    }
}

/**
 *  Read a file entry as text data after removed the trailing nuls
 */
p2FormatReader.prototype.getEntryAsTextTrimmed = function(entryKey, callback, context) {
    if (typeof context === 'undefined') { context = this; }

    var entryIndexInfo = context.index[entryKey];

    if (typeof entryIndexInfo !== 'undefined') {
      var start = context.dataOffset + entryIndexInfo.offset * context.blockSize;
      var end = context.dataOffset + (entryIndexInfo.offset + entryIndexInfo.size) * context.blockSize;

      context.filereader.readRangeAsText(start,end, function(text) {
              var dataLength =  DataControllerFile.prototype.getNullTerminatedStringLength(text);
              var textTrimmed = text.slice(0, dataLength);
              //var allData = JSON.parse(textTrimmed);
              callback(textTrimmed);
      });
    } else {
      throw new RuntimeException(STATIC_FILE_FIELD_MISSING, 'Unknown index: '.concat(entryKey))
    }
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
 * @param entryKey the key of the entry to get the bytes form
 * @param start start offset in the entry
 * @param length number of bytes to retrive 
 */
p2FormatReader.prototype.getBytesInEntry = function(entryKey, start, length, callback, context) {
    // TODO
    if (typeof context === 'undefined') {context = this;}

    var entryIndexInfo = context.index[entryKey];
    var start = context.dataOffset + entryIndexInfo.offset * context.blockSize + start;
    var end = start + length;


    context.filereader.readRange(start,end, function(data) {
	    callback(data);
    });
}

/**
 * Returns true if the underlying connection supports requests
 * that retrieve multiple regions of the file at once and false otherwise
 */
p2FormatReader.prototype.supportsMultiRequest = function() {
  return this.filereader.supportsMultiRequest();
};

/** 
 * Returns multiple ranges from the specified entry using a single request
 * @param entryKey the key of the entry to get the data from
 * @param ranges an array describing the ranges requested, an array of two elements
 * the first is the starting offset the second is the length
 * @param finishCallback callback function to call when done
 * @param progressCallback callback function to call to provide updates on download progess
 */
p2FormatReader.prototype.getMultiBytesInEntry = function(entryKey, ranges, finishCallback, progressCallback, context) {
  if (typeof context === 'undefined') {context = this;}
  
  if(this.supportsMultiRequest()) {
    // Calculate the entry offset
    var entryIndexInfo = context.index[entryKey];
    var entryOffset = context.dataOffset + entryIndexInfo.offset * context.blockSize;
    
    // Calculate the ranges that we need from
    var rangeList = [];
    for (var i = 0; i < ranges.length; i++) {
      var rangeStart = entryOffset + ranges[i][0];
      var rangeEnd = entryOffset + ranges[i][0] + ranges[i][1];
      rangeList[i] = [rangeStart, rangeEnd];
    }

    context.filereader.readMultiRange(rangeList, finishCallback,progressCallback);

  } else {
    // This is an error in the upstream code, should have never called this on this object
    throw new RuntimeException(FUNCTIONALITY_NOT_SUPPORTED,"Called getMultiBytesInEntry() on an format reader than does not support multi-range retrieval!")
  }
};
