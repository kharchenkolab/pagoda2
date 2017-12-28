"use strict";

/*
 * Filename: DataControllerServer.js
 * Author: Nikolas Barkas
 * Date: March 2017
 * Description: DataControllerServer object for pagoda2
 */

/**
 * Implementation for data controller for rook server backed interaction
 * @constructor
 */
function DataControllerServer() {
    // Init the cache
    this.initCache();
};

/**
 * Initialize the cache
 */
DataControllerServer.prototype.initCache = function() {
    this.cache = {};
    this.cache["cellmetadata"] = null;
    this.cache["cellorder"] = null;
    this.cache["reduceddendrogram"] = null;
    this.cache["embeddings"] = [];
    this.cache["lastexpressionmatrix"] = null;
    this.cache["lastaspectmatrix"] = null;
}

/**
 * Get the reduced dendrogram and the number  of cells in each group (required for plotting)
 * @param callback the callback function to call when you have the data
 */
DataControllerServer.prototype.getReducedDendrogram = function(callback) {
    if (this.cache['reduceddendrogram'] === null) {
    	$.ajax({
    	    type: "GET",
    	    dataType: "json",
    	    data: { "dataidentifer": "reduceddendrogram" },
    	    url: "getData.php?dataidentifier=reduceddendrogram",
    	    success: function(data) {
    		callback(data);
    	    }
    	});
    } else {
	callback(this.cache['reduceddendrogram']);
    }
}

/**
 * Get the cell identifiers in the default order
 */
DataControllerServer.prototype.getCellOrder = function(callback) {
  var dataCntr = this;

  if (this.cache["cellorder"] === null) {
  	$.ajax({
  	    type: "GET",
  	    dataType: "json",
  	    url: "getData.php",
  	    data: { "dataidentifier": "cellorder" },
  	    success: function(data) {
    		  dataCntr.cache["cellorder"] = data;
    		  callback(data);
	      } // success
    }); //ajax
  } else {
    callback(this.cache["cellorder"]);
  }
}

/**
 * Get the hierarchy of embeddings and reductions
 */
DataControllerServer.prototype.getEmbeddingStructure = function(callback) {
  	$.ajax({
  	    type: "GET",
  	    dataType: "json",
  	    url: "getData.php",
  	    data: { "dataidentifier": "embeddingstructure" },
  	    success: function(data) {
    		  callback(data);
	      } // success
    }); //ajax
}

/**
 * Get an extjs store with
 * the available aspects
 */
DataControllerServer.prototype.getAvailableAspectsStore = function(callback) {
  $.ajax({
	    type: "GET",
	    dataType: "json",
	    url: "getData.php",
	    data: {'dataidentifier':'availableaspects'},
	    success: function(data) {

        // Data needs to be converted into an array
        // of objects with named properties
        var dataStructured = [];
        for (var i = 0; i < data.length; i++) {
          dataStructured[i] = {'name': data[i]};
        };

	      var pagingStore = Ext.create('LocalJsonStore', {
	        autoLoad: true,
	        model: 'aspectTableEntry',
	        pageSize: 20,
	        localData: dataStructured
	      });
	      callback(pagingStore);

	   }
  });
}

/**
 * Get the available genesets in the selected aspect
 * @return ajax request
 */
DataControllerServer.prototype.getAvailableGenesetsInAspectStore = function(aspectId, callback) {
  var ajaxReq = $.ajax({
    dataType:'json',
    type: 'GET',
    url: 'getData.php',
    data: {
      'dataidentifier': 'genesetsinaspect',
      'aspectId': aspectId
    },
    success: function(data) {
      var pagingStore = Ext.create('LocalJsonStore', {
        autoLoad: true,
        model: 'genesetInAspectEntry',
        pageSize: 100,
        localData: data
      });

      callback(pagingStore);

    }
  })

  // return the ajax request so that it can be cancelled if required
  return ajaxReq;
}

/**
 * Get a dgCMatrixReader with the cell aspect weights for the specified
 * cell index range and all aspects
 */
DataControllerServer.prototype.getAspectMatrix = function(cellIndexStart, cellIndexEnd, getCellNames, callback) {
  var dataCntr = this;

  // Does the function return via callback
  function doReturn(data) {
		// Unpack the data
		var x = dataCntr.unpackCompressedBase64Float64Array(data.x);
		var i = dataCntr.unpackCompressedBase64Int32Array(data.i);
		var p = dataCntr.unpackCompressedBase64Int32Array(data.p);

		// This is a fix for the way R toJSON encodes one element arrays
		if (typeof data.Dimnames1 === 'string') {data.Dimnames1 = [ data.Dimnames1 ]; }
		if (typeof data.Dimnames2 === 'string') {data.Dimnames2 = [ data.Dimnames2 ]; }

		//Convert to matrix reader and return
		var m = new dgCMatrixReader(i, p, data.Dim, data.Dimnames1,
					    data.Dimnames2, x);
		callback(m);
  }

  // Check input
  if (!Number.isInteger(cellIndexStart)) { throw new Error("cellIndexStart must be an integer"); }
	if (!Number.isInteger(cellIndexEnd)) { throw new Error("cellIndexEnd must be an interger"); }

	// Check the cache
	if(this.cache.lastaspectmatrix !== null ) {
	    if ( this.cache.lastaspectmatrix.cellindexstart === cellIndexStart &
  		 this.cache.lastaspectmatrix.cellindexend === cellIndexEnd &
  		 this.cache.lastaspectmatrix.getCellNames === getCellNames) {
    		doReturn(this.cache.lastaspectmatrix.data);
    		return null; // No ajax request to return
	    }
	}

  // Setup the request data
	var requestData = {
	    "dataidentifier": "aspectmatrixsparsebyindexbinary",
	    "cellindexstart": cellIndexStart,
	    "cellindexend": cellIndexEnd,
	    "getcellnames": getCellNames
	};


	var ajaxRequest = $.ajax({
	    type: "GET",
	    dataType: "json",
	    url: "getData.php",
	    data: requestData,
	    success: function(data) {

		// Check if the returned data are valid
		if (typeof data !== 'object') {
		    throw new Error('Returned data is not of type object');
		}
		if (! data.hasOwnProperty('i') ){
		    throw new Error('data object does not have an i field');
		}
		if (! data.hasOwnProperty('p')) {
		    throw new Error('data object does not have a p field');
		}
		if (! data.hasOwnProperty('Dim')) {
		    throw new Error('data object does not have a Dim field');
		}
		if (! data.hasOwnProperty('Dimnames1')) {
		    throw new Error('data object does not have Dimnames1 field');
		}
		if (! data.hasOwnProperty('Dimnames2')){
		    throw new Error('data object does not have Dimnames2 field');
		}
		if (! data.hasOwnProperty('x')) {
		    throw new Error('data object does not have x field');
		}

		// Update the cache
		dataCntr.cache.lastaspectmatrix = {};
		dataCntr.cache.lastaspectmatrix.cellindexstart = cellIndexStart;
		dataCntr.cache.lastaspectmatrix.cellindexend = cellIndexEnd;
		dataCntr.cache.lastaspectmatrix.getCellNames = getCellNames;
		dataCntr.cache.lastaspectmatrix.data = data;

		doReturn(data);
	    }
	});

	return ajaxRequest;
}

/** 
 * Get all expression values for the specified cells
 * @param cellNames names of cells to get
 * @param callback the function to call when complete
 * @param progressCallback not implemented
 */
DataControllerServer.prototype.getExpressionValuesSparseByCellName = function(cellNames, callback, progressCallback){
	// Setup the request data
	var requestData = {
	    "dataidentifier": "expressionmatrixsparsebycellname",
	    "cellnames": cellNames.join('|')
	};
	
  var dataCntr = this;
	
	// Does the function return via callback, factored out to avoid duplication
	function doReturn(data) {
		// Unpack the data
		var x = dataCntr.unpackCompressedBase64Float64Array(data.x);
		var i = dataCntr.unpackCompressedBase64Int32Array(data.i);
		var p = dataCntr.unpackCompressedBase64Int32Array(data.p);

		// This is a fix for the way R toJSON encodes one element arrays
		if (typeof data.Dimnames1 === 'string') {data.Dimnames1 = [ data.Dimnames1 ]; }
		if (typeof data.Dimnames2 === 'string') {data.Dimnames2 = [ data.Dimnames2 ]; }

		//Convert to matrix reader and return
		var m = new dgCMatrixReader(i, p, data.Dim, data.Dimnames1,
					    data.Dimnames2, x);

		callback(m);
	}

	var ajaxRequest = $.ajax({
	    type: "POST",
	    dataType: "json",
	    url: "getData.php?dataidentifier=expressionmatrixsparsebycellname",
	    data: requestData,
	    success: function(data) {
        doReturn(data);
	    }
	});

	return ajaxRequest;
}

/**
 * Get a subset of the expression matrix specified but only
 * transfer lz compressed binary array data. Cells are specified
 * using indices on the basis of the default cell order
 * @param geneIds the gene identifiers
 * @param cellIds the cell identifiers
 * @param callback callback function
 * @return a jQuery ajax request object
 */
DataControllerServer.prototype.getExpressionValuesSparseByCellIndexUnpacked =
    function(geneIds, cellIndexStart, cellIndexEnd, getCellNames, callback) {

	var dataCntr = this;

	// Does the function return via callback, factored out to avoid duplication
	function doReturn(data) {
		// Unpack the data
		var x = dataCntr.unpackCompressedBase64Float64Array(data.x);
		var i = dataCntr.unpackCompressedBase64Int32Array(data.i);
		var p = dataCntr.unpackCompressedBase64Int32Array(data.p);

		// This is a fix for the way R toJSON encodes one element arrays
		if (typeof data.Dimnames1 === 'string') {data.Dimnames1 = [ data.Dimnames1 ]; }
		if (typeof data.Dimnames2 === 'string') {data.Dimnames2 = [ data.Dimnames2 ]; }

		//Convert to matrix reader and return
		var m = new dgCMatrixReader(i, p, data.Dim, data.Dimnames1,
					    data.Dimnames2, x);

		callback(m);
	}

	// Check input
	if (!Array.isArray(geneIds)) { throw new Error("geneIds must be an array of strings");	}
	if (!Number.isInteger(cellIndexStart)) { throw new Error("cellIndexStart must be an integer"); }
	if (!Number.isInteger(cellIndexEnd)) { throw new Error("cellIndexEnd must be an interger"); }

	// Check the cache
	if(this.cache.lastexpressionmatrix !== null ) {
	    if ( this.cache.lastexpressionmatrix.cellindexstart === cellIndexStart &
    		 this.cache.lastexpressionmatrix.cellindexend === cellIndexEnd &
    		 this.cache.lastexpressionmatrix.getCellNames === getCellNames &
    		 pagHelpers.compareArrays1d(geneIds, this.cache.lastexpressionmatrix.geneIds)) {

    		doReturn(this.cache.lastexpressionmatrix.data);
    		return null;
	    }
	}

	// Setup the request data
	var requestData = {
	    "dataidentifier": "expressionmatrixsparsebyindexbinary",
	    "geneids": geneIds.join('|'),
	    "cellindexstart": cellIndexStart,
	    "cellindexend": cellIndexEnd,
	    "getCellNames": getCellNames
	};

	var ajaxRequest = $.ajax({
	    type: "POST",
	    dataType: "json",
	    url: "getData.php?dataidentifier=expressionmatrixsparsebyindexbinary",
	    data: requestData,
	    success: function(data) {

		// Check if the returned data are valid
		if (typeof data !== 'object') {
		    throw new Error('Returned data is not of type object');
		}
		if (! data.hasOwnProperty('i') ){
		    throw new Error('data object does not have an i field');
		}
		if (! data.hasOwnProperty('p')) {
		    throw new Error('data object does not have a p field');
		}
		if (! data.hasOwnProperty('Dim')) {
		    throw new Error('data object does not have a Dim field');
		}
		if (! data.hasOwnProperty('Dimnames1')) {
		    throw new Error('data object does not have Dimnames1 field');
		}
		if (! data.hasOwnProperty('Dimnames2')){
		    throw new Error('data object does not have Dimnames2 field');
		}
		if (! data.hasOwnProperty('x')) {
		    throw new Error('data object does not have x field');
		}

		// Update the cache
		dataCntr.cache.lastexpressionmatrix = {};
		dataCntr.cache.lastexpressionmatrix.cellindexstart = cellIndexStart;
		dataCntr.cache.lastexpressionmatrix.cellindexend = cellIndexEnd;
		dataCntr.cache.lastexpressionmatrix.getCellNames = getCellNames;
		dataCntr.cache.lastexpressionmatrix.geneIds = geneIds;
		dataCntr.cache.lastexpressionmatrix.data = data;

		doReturn(data);
	 }
	});

	return ajaxRequest;
} // function

/**
 * Get the available embeddings for the data reduction type
 * @param type the reduction type for which to get embeddings
 * @param callback the callback to call
 * @param callbackParamers parameters to pass to the callback
 */
DataControllerServer.prototype.getAvailableEmbeddings =  function(type, callback, callbackParams ) {
    $.ajax({
	dataType: "json",
	url: "getData.php",
	data: {'dataidentifier': 'availableembeddings', 'type': type},
	success: function(data) {
	    // The returned data can be:
	    //  * a string
	    //  * an array of strings
	    //  * null

	    if (!( data === null | typeof data === 'string' | Array.isArray(data) )) {
		throw new Error('data needs to be one of null, string or array');
	    }
	    // If an array check that it is not empty and that all elements are strings
	    if (Array.isArray(data)) {
		if(data.length === 0) {
		    throw new Error('data is an array of length 0');
		}
		for (var i = 0; i < data.length; i++) {
		    if (typeof data[i] !== 'string') {
			throw new Error('Element ' + i + 'of data is not a string');
		    }
		}
	    }

	    // NOTE if only one value is returned the rjson library will
	    // just return the string literal and this will cause problems later
	    // Checking and fixing:
	    if (typeof data === "string") {
	       data = [data];
	    }

	    callback(data, callbackParams);
	}
    });
};

/**
 * Get an embedding for a specific reduction type
 * @param type the reduction type
 * @param embeddingType the embedding type to get
 * @param callback the callback to get
 */
DataControllerServer.prototype.getEmbedding = function(type, embeddingType, callback) {
    var cacheId =  type + '_' + embeddingType;

    if (typeof this.cache.embeddings[cacheId] !== 'undefined') {
	    var data = this.cache.embeddings[cacheId];
	    callback(data);
    } else {
      	var dataCntr = this;
    	$.ajax({
    	    dataType: "json",
    	    url: "getData.php",
    	    data: {'dataidentifier': 'embedding', 'type': type, 'embeddingtype': embeddingType },
    	    success: function(data) {
        		// The data is returned in serialised array format
        		// Do some checks to ensure we got what we wanted
        		if (! data.hasOwnProperty('values')) {
        		    throw new Error('data does not have a values field');
        		}
        		if (! data.hasOwnProperty('dim')) {
        		    throw new Error('data does not have dim field');
        		}
        		if (! data.hasOwnProperty('rownames')) {
        		    throw new Error('data does not have a rownames field');
        		}
        		if (!data.hasOwnProperty('colnames')) {
        		    throw new Error('data does not have colnames field');
        		}
        		// if (!Array.isArray(data.values)) {
        		// 	throw new Error('data.values is not an array');
        		// }
        		if (!Array.isArray(data.dim)) {
        		    throw new Error('data.dim is not an array');
        		}
        		if (data.dim.length !== 2) {
        		    throw new Error('data.dim is not of length 2');
        		}

        		data.values = dataCntr.unpackCompressedBase64Float64Array(data.values);
        		data = pagHelpers.jsonSerialisedToArrayOfArrays(data);
            dataCntr.cache.embeddings[cacheId] = data;

    		    callback(data);
    	    }
    	}); // ajax
    }
};

/**
 * Get the available reduction types
 * @param the callback
 */
DataControllerServer.prototype.getAvailableReductionTypes = function(callback) {
    $.ajax({
	dataType: "json",
	url: "getData.php",
	data: {'dataidentifier': 'availablereductiontypes' },
	success: function(data) {
	    // If there is only one R sends it as a string not
	    // as an array of length 1
	    if (typeof data === "string") {
	       data = [data];
	    }

	    callback(data);
	}
    });
};

/**
 * Get the cell metadata
 * @param callback callback funciton
 * @param callbackParameters data to pass to the callback function
 */
DataControllerServer.prototype.getCellMetadata = function(callback, callbackParameters) {
    var dataCntr = this;


    if (dataCntr.cache["cellmetadata"] !== null) {
    	// If the data is in the  cache don't make another request
    	callback(dataCntr.cache["cellmetadata"], callbackParameters);
    } else {
	$.ajax({
	    dataType: "json",
	    type: "POST",
	    url: "getData.php?dataidentifier=cellmetadata",
	    success: function(data) {
		if ( typeof data !== 'object') {
		    throw new Error('data is not of type object');
		}

		// Check all entries for correct format
		for ( var key in data ) {
		    if( typeof data[key]['data'] !== 'object' ) {
			throw new Error('data member of the metadata entry ' + key + ' is not an object');
		    }
		    if ( data[key]['data'].length === 0) {
			throw new Error('data member of the metadata entry ' + key + ' is empty');
		    }


		    if ( typeof data[key]['palette'] !== 'object' ){
			throw new Error('palette member of the metadata entry ' + key + ' is not an object');
		    }
		    if (data[key]['palette'].length === 0 ) {
			throw new Error('palette member of the metadata entry ' + key + ' is empty');
		    }

		}

		// TODO: Check all meta data contain the same number of entries
		// TODO: Check that all the values have a palette match

		dataCntr.cache["cellmetadata"] = data;
		callback(data,callbackParameters);
	    }
	});
    }
};

/**
 * Get an Extjs store for a list of genes that
 * belong to the specified geneset
 * @param name the name of the geneset the list of which to retrieve
 */
DataControllerServer.prototype.getGeneSetStoreByName = function(name, callback) {
    $.ajax({
    	dataType: 'json',
    	url: 'getData.php?dataidentifier=genesetgeneinformation&genesetname=' + name,
    	success: function(data) {
    	    var pagingStore = Ext.create('LocalJsonStore', {
        		autoLoad: true,
        		model: 'geneTableEntry',
        		pageSize: 100,
        		localData: data
    	    });
    	    callback(pagingStore);
    	}
    });
}


/**
 * get an extjs information store for the
 * genesets
 */
DataControllerServer.prototype.getGeneSetInformationStore = function(callback) {
    $.ajax({
    	dataType: 'json',
    	url: 'getData.php?dataidentifier=availablegenesets',
    	success: function(data) {
    	    var pagingStore = Ext.create('LocalJsonStore', {
    		autoLoad: true,
    		model: 'geneSetTableEntry',
    		pageSize: 100,
    		localData: data
    	    });
    	    callback(pagingStore);
    	}
    });
}


/**
 * Get an ExtJS proxy object for connecting to the GeneTable.
 * @description return a extjs store object with the gene table
 * information data
 */
DataControllerServer.prototype.getGeneInformationStore = function(callback) {
    $.ajax({
    	dataType: 'json',
    	url: 'getData.php?dataidentifier=geneinformation',
    	success: function(data) {
    	    var pagingStore = Ext.create('LocalJsonStore', {
    		autoLoad: true,
    		model: 'geneTableEntry',
    		pageSize: 100,
    		localData: data,
    	    });
    	    pagingStore.sort('dispersion', 'DESC');
    	    callback(pagingStore);
    	}
    });
};

/**
 * Get part of the aspect matrix specifying both the aspects to get and the cell start/end indices
 * @cellIndexStart start cell index
 * @cellIndexEnd end cell index
 * @aspectIds the ids of the aspect to get
 * @getCellNames logical flag denoting if the cell names are to be received
 * @callback callback function
 */
DataControllerServer.prototype.getAspectMatrixByAspect = function(cellIndexStart, cellIndexEnd, aspectIds, callback) {
    // Check input
    if (!Array.isArray(aspectIds)) {
	    throw new Error("aspectIds must be an array of strings");
    }

    if (!Number.isInteger(cellIndexStart)) {
	    throw new Error("cellIndexStart must be an integer");
    }

    if (!Number.isInteger(cellIndexEnd)) {
	    throw new Error("cellIndexEnd must be an interger");
    }

    // Setup the request data
    var requestData = {
    	"dataidentifier": "aspectmatrixbyaspect",
    	"aspectids": aspectIds.join('|'),
    	"cellindexstart": cellIndexStart,
    	"cellindexend": cellIndexEnd,
    };

    var request = $.ajax({
    	type: "POST",
    	dataType: "json",
    	url: "getData.php?dataidentifier=aspectmatrixbyaspect",
    	data: requestData,
    	success: function(data) {


	    // Check if the returned data are valid
	    if (typeof data !== 'object') {
		    throw new Error('Returned data is not of type object');
	    }
	    if (! data.hasOwnProperty('i') ){
		    throw new Error('data object does not have an i field');
	    }
	    if (! data.hasOwnProperty('p')) {
		    throw new Error('data object does not have a p field');
	    }
	    if (! data.hasOwnProperty('Dim')) {
		    throw new Error('data object does not have a Dim field');
	    }
	    if (! data.hasOwnProperty('Dimnames1')) {
		    throw new Error('data object does not have Dimnames1 field');
	    }
	    if (! data.hasOwnProperty('Dimnames2')){
		    throw new Error('data object does not have Dimnames2 field');
	    }
	    if (! data.hasOwnProperty('x')) {
		    throw new Error('data object does not have x field');
	    }

	    var dataCntr = new DataControllerServer();
	    var x = dataCntr.unpackCompressedBase64Float64Array(data.x);
	    var i = dataCntr.unpackCompressedBase64Int32Array(data.i);
	    var p = dataCntr.unpackCompressedBase64Int32Array(data.p);

	    // This is a fix for the way R toJSON encodes one element arrays
	    if (typeof data.Dimnames1 === 'string') {
		    data.Dimnames1 = [ data.Dimnames1 ];
	    }

	    if (typeof data.Dimnames2 === 'string') {
		    data.Dimnames2 = [ data.Dimnames2 ]
	    }

	    //Convert to full matrix and return
	    var m = new dgCMatrixReader(i, p, data.Dim, data.Dimnames1, data.Dimnames2, x);
	      callback(m.getFullMatrix());
	    }
    });

    // For allowing cancellation
    return request;
}

DataControllerServer.prototype.getAppMetadata = function(callback) {
    var request = $.ajax({
    	dataType: "json",
    	url: "getData.php?dataidentifier=appmetadata",
    	success: function(data) {
        callback(data);
    	}
    });
}

DataControllerServer.prototype.getGeneNeighbours = function(queryGenes, callback) {
    // Setup the request data
    var requestData = {
      dataidentifier: "relatedgenes",
    	"querygenes": queryGenes
    };

    var request = $.ajax({
    	type: "POST",
    	dataType: "json",
    	url: "getData.php?dataidentifier=relatedgenes",
    	data: requestData,
    	success: function(data) {
        callback(data);
    	}
    });
}

/**
 * Given a string that encodes a sequence of lz compressed
 * Int32 values, decompress and return resulting typed array
 * @param encoded the encoded array
 * @private
 * @return Int32Array with decoded values
 */
DataControllerServer.prototype.unpackCompressedBase64Int32Array = function(encoded) {
    var compressed = atob(encoded);
    var raw = pako.inflate(compressed);
    var int32aBuffer = new Uint8Array(raw);
    var int32a = new Int32Array(int32aBuffer.buffer);

    return int32a;
}

/**
 * Given a string that encodes a sequence of lz compressed
 * Float64 values, decompress and return resulting typed array
 * @param encoded the encoded array
 * @private
 * @return Float64Array with decoded values
 */
DataControllerServer.prototype.unpackCompressedBase64Float64Array =  function (encoded) {
    // Unpack base 64
    var compressed = atob(encoded);
    var raw = pako.inflate(compressed);
    var f64aBuffer = new Uint8Array(raw);
    var f64a = new Float64Array(f64aBuffer.buffer);

    return f64a;
}



