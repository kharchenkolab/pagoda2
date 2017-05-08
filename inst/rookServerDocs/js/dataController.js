/*
 * Filename: dataController.js
 * Author: Nikolas Barkas
 * Date: March 2017
 * Description: dataController object for pagoda2
 */

/**
 * Handles all data requests
 * @constructor
 * @description At the moment these will be simple callbacks to the backend
 * Rook server. In the future this will be loaded from a local
 * file or it can be requested from an external resource database (cross-site
 * scripting security issues may arise). It can also cache requests.
 * @param repository this is the data source currently only remote is supported this can alternatively be a local
 */
function dataController(repository) {
    if (typeof dataController.instance === 'object') {
	return dataController.instance;
    };

    // Init variables here
    this.repository = repository;

    // Init the cache
    this.cache = {};
    this.cache["cellmetadata"] = null;
    this.cache["cellorder"] = null;
    this.cache["reduceddendrogram"] = null;
    this.cache["embeddings"] = [];

    this.cache["lastexpressionmatrix"] = null;
    this.cache["lastaspectmatrix"] = null;

    // Define extjs objects -- used for the table data
    // where extjs controls are used
    this.defineExtJsObjects();

    dataController.instance = this;
};

/**
 * Returns the named hierarchy in the hclust format
 * @description This is currently a partial implementation as it will
 * just return the 'dummy' hierarchy. Can simply change this to be called main
 * if we want to use only one hierarchy
 * @param name Name of the hierarchy to return
 * @param callback a callback function to call when the data is back
 */
dataController.prototype.getHierarchy =  function(name, callback) {
    console.error('Deprecated function dataController.getHierarchy called, use getReducedDendrogram()');

    // TODO: Implement function in full to support multiple hierarchies
    var dataCntr = new dataController();

    // Cache hierarchies
    if (name in dataCntr.cache["hierarchies"]) {
	// It's in the cache already
	callback(dataCntr.cache["hierarchies"][name]);
    } else {
	$.ajax({
	    dataType: "json",
	    url: "getData.php",
	    data: {'dataidentifier': 'hierarchy', 'type': name },
	    success: function(data) {
		var dataCntr = new dataController();

		// The merge array values refer to a 1 indexed table
		// fix this as the js tables are 0 indexed
		for (var i =0; i < data.merge.length; i++) {
		    // This is the case only for internal nodes
		    if (data.merge[i] > 0) {
			data.merge[i] = data.merge[i] - 1;
		    }
		}

		dataCntr.cache["hierarchies"][name] = data;

		callback(data);
	    }
	});
    };
};

/**
 * Get the reduced dendrogram and the number  of cells in each group (required for plotting)
 * @param callback the callback function to call when you have the data
 */
dataController.prototype.getReducedDendrogram = function(callback) {
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
dataController.prototype.getCellOrder = function(callback) {
    if (this.cache["cellorder"] === null) {
	$.ajax({
	    type: "GET",
	    dataType: "json",
	    url: "getData.php",
	    data: { "dataidentifier": "cellorder" },
	    success: function(data) {
		var dataCntr = new dataController();
		dataCntr.cache["cellorder"] = data;
		callback(data);
	    } // success
	}); //ajax
    } else {
	callback(this.cache["cellorder"]);
    }
}

/**
 * Get an extjs store with
 * the available aspects
 */
dataController.prototype.getAvailableAspectsStore = function(callback) {
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
dataController.prototype.getAvailableGenesetsInAspectStore = function(aspectId, callback) {
  var ajaxReq = $.ajax({
    type: 'GET',
    dataType:'json',
    url: 'getData.php',
    data: {
      'dataidentifier': 'genesetsinaspect',
      'aspectId': aspectId
    },
    success: function(data) {

      var dataStructured = [];
      for(var i = 0; i < data.length; i++){
        dataStructured[i] = {'name': data[i]};
      };

      var pagingStore = Ext.create('LocalJsonStore', {
        autoLoad: true,
        model: 'genesetInAspectEntry',
        pageSize: 50,
        localData: dataStructured
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
dataController.prototype.getAspectMatrix = function(cellIndexStart, cellIndexEnd, getCellNames, callback) {
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
 * Get a subset of the expression matrix specified but only
 * transfer lz compressed binary array data. Cells are specified
 * using indices on the basis of the default cell order
 * @param geneIds the gene identifiers
 * @param cellIds the cell identifiers
 * @param callback callback function
 * @return a jQuery ajax request object
 */
dataController.prototype.getExpressionValuesSparseByCellIndexUnpacked =
    function(geneIds, cellIndexStart, cellIndexEnd, getCellNames = true, callback) {
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
	    "geneids": geneIds,
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
 * Get part of the main expression matrix in sparse format
 * with the genes specified as an array of strings and the cells
 * specifies as start and end indeces on the default order
 * @geneIds array of cell indexes
 * @cellIndexStart start index for cells
 * @cellIndexEnd end index for cells
 * @callback callback function
 * @return the jQuery ajax object
 */
dataController.prototype.getExpressionValuesSparseByCellIndex = function(geneIds, cellIndexStart, cellIndexEnd, callback) {
       return this.getExpressionValuesSparseByCellIndexBinary(geneIds, cellIndexStart, cellIndexEnd,callback)
}



/**
 * Get part of the main expression matrix in sparse format
 * with the genes specified as an array of strings and the cells
 * specified as start and end indeces on the default order
 * @descriptions This  function performs the transfer by encoding
 * the i p and x arrays as compressed binary arrays of either int32 or float64
 * @private
 * @geneIds array of cell indexes
 * @cellIndexStart start index for cells
 * @cellIndexEnd end index for cells
 * @callback callback function
 * @return the ajax object
 */
dataController.prototype.getExpressionValuesSparseByCellIndexBinary = function(geneIds, cellIndexStart, cellIndexEnd, callback) {

    // Check input
    if (!Array.isArray(geneIds)) {
	throw new Error("geneIds must be an array of strings");
    }

    if (!Number.isInteger(cellIndexStart)) {
	throw new Error("cellIndexStart must be an integer");
    }

    if (!Number.isInteger(cellIndexEnd)) {
	throw new Error("cellIndexEnd must be an interger");
    }

    // Setup the request data
    var requestData = {
	"dataidentifier": "expressionmatrixsparsebyindexbinary",
	"geneids": geneIds,
	"cellindexstart": cellIndexStart,
	"cellindexend": cellIndexEnd,
	"getCellNames": true
    };

    var request = $.ajax({
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

	    var dataCntr = new dataController();
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

    return request;
}

/**
 * Given a string that encodes a sequence of lz compressed
 * Int32 values, decompress and return resulting typed array
 * @param encoded the encoded array
 * @return Int32Array with decoded values
 */
dataController.prototype.unpackCompressedBase64Int32Array = function(encoded) {
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
 * @return Float64Array with decoded values
 */

dataController.prototype.unpackCompressedBase64Float64Array =  function (encoded) {
    // Unpack base 64
    var compressed = atob(encoded);
    var raw = pako.inflate(compressed);
    var f64aBuffer = new Uint8Array(raw);
    var f64a = new Float64Array(f64aBuffer.buffer);

    return f64a;
}



/**
 * Get the available embeddings for the data reduction type
 * @param type the reduction type for which to get embeddings
 * @param callback the callback to call
 * @param callbackParamers parameters to pass to the callback
 */
dataController.prototype.getAvailableEmbeddings =  function(type, callback, callbackParams ) {
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
dataController.prototype.getEmbedding = function(type, embeddingType, callback) {
    var cacheId =  type + '_' + embeddingType;

    if (typeof this.cache.embeddings[cacheId] !== 'undefined') {
	data = this.cache.embeddings[cacheId];

	// NOTE: We dont' need to decompress here because the object was not a deep copy when
	// created. We would like it to have been a deep copy so that we conserve memory
	// in the cache.

	// var dataCntr = new dataController();
	// var unpackedValues = dataCntr.unpackCompressedBase64Float64Array(data.values);
	// data.values = unpackedValues;

	callback(data);
    } else {
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

		var dataCntr = new dataController();

		dataCntr.cache.embeddings[cacheId] = data;

		// TODO: Check that the arrays contain numbers

		var unpackedValues = dataCntr.unpackCompressedBase64Float64Array(data.values);
		data.values = unpackedValues;

		callback(data);
	    }
	}); // ajax
    }
};

/**
 * Get the available reduction types
 * @param the callback
 */
dataController.prototype.getAvailableReductionTypes = function(callback) {
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
 */
dataController.prototype.getCellMetadata = function(callback) {
    var dataCntr = new dataController();


    if (dataCntr.cache["cellmetadata"] !== null) {
	// If the data is in the  cache don't make another request
	callback(dataCntr.cache["cellmetadata"]);
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
		callback(data);
	    }
	});
    }
};

/**
 * Get an Extjs store for a list of genes that
 * belong to the specified geneset
 * @param name the name of the geneset the list of which to retrieve
 */
dataController.prototype.getGeneSetStoreByName = function(name, callback) {
    if (this.repository === 'remote') {
	return this.getGeneSetStoreByNameRemote(name, callback);
    } else if (this.repository === 'local'){
	return this.getGeneSetStoreByNameLocal(name, callback);
    } else {
	console.error('Unknown repository');
    }
}

/**
 * Get an extjs memory store that contains all the genes
 * that belong to the specified gene set
 * @param name the name of the geneset
 * @private
 */
dataController.prototype.getGeneSetStoreByNameLocal = function(name, callback) {
    console.error('dataController.getGeneSetStoreByNameLocal() not implemented');
}

/**
 * Get an extjs memory store that contrains all the genes
 * that belong to the specified gene set from a remote repos
 * @param name name of the geneset
 * @private
 */
dataController.prototype.getGeneSetStoreByNameRemote = function(name, callback) {

    $.ajax({
	dataType: 'json',
	url: 'getData.php?dataidentifier=genesetgeneinformation&genesetname='+name,
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
dataController.prototype.getGeneSetInformationStore = function(callback) {
    if (this.repository === 'remote') {
	return this.getGeneSetInformationStoreRemote(callback);
    } else if (this.repository === 'local'){
	return this.getGeneSetInformationStoreLocal(callback);
    } else {
	console.error('Unknown repository');
    }
}

/**
 * Get an extjs information store for the
 * gene sets table using the server backedn
 */
dataController.prototype.getGeneSetInformationStoreRemote = function(callback) {
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
 * Get a custom extJS object of type LocalJsonStore
 * with the overdispersed genes. Return via callback
 */
dataController.prototype.getOdGeneInformationStore = function(callback) {
    if (this.repository === 'remote') {
	return this.getRemoteOdGeneInformationStore(callback);
    } else if (this.repository === 'local') {
	return this.getLocalOdGeneInformationStore(callback);
    } else {
	console.error('Unknown repository');
    }
};

/**
 * Get a custom extJS object of type LocalJsonStore
 * with the overdispersed genes, with data from a remote server.
 * Return via callback
 * @private
 */
dataController.prototype.getRemoteOdGeneInformationStore = function(callback) {
    $.ajax({
	dataType: "json",
	url:'getData.php?dataidentifier=odgeneinformation',
	success: function(data) {
	    var pagingStore = Ext.create('LocalJsonStore', {
		autoLoad: true,
		model: "geneTableEntry", // Use the same model as in gene table for themoment
		pageSize: 100,
		localData: data
	    });
	    callback(pagingStore);
	}
    });
}

/**
 * Get a local store for the od genes table
 * @private
 */
dataController.prototype.getLocalOdGeneInformationStore = function() {
    console.error('getLocalOdGeneInformationStore not implemented!');
}

/**
 * Get an ExtJS proxy object for connecting to the GeneTable.
 * @description return a extjs store object with the gene table
 * information data
 */
dataController.prototype.getGeneInformationStore = function(callback) {
    if (this.repository === 'remote') {
	return this.getRemoteGeneInformationStore(callback);
    } else if (this.repositor === 'local') {
	return this.getLocalGeneInformationStore(callback);
    } else {
	console.error('Unknown repository');
    }
};

/**
 * Return a ExtJS store object configured to get data from a local
 * file store
 * @private
 */
dataController.prototype.getLocalGeneInformationStore =  function() {
    // TODO: Implement me
    console.error("dataController.getLocalGeneInformationStore not implemented!");
};

/**
 * Define extJS models and objects required by the data controller.
 * All data models that involve the data controller should
 * be defined here
 * @description called once during the singleton constructor
 * @private
 */
dataController.prototype.defineExtJsObjects = function() {

    // NOTE: We use this extension of the memory
    // store to allow sorting and pagination at the
    // same time
    Ext.define('LocalJsonStore', {
	extend: 'Ext.data.Store',
	constructor: function(config) {
	    config.autoLoad = true;
	    config.remoteSort = true;
	    config.remoteFilter = true;
	    config.proxy = {
		type: 'memory',
		enablePaging: true,
		data: config.localData,
		reader: {
		    type: 'json'
		},
		writer: config.writerConfig ? config.writerConfig : { type: 'json', allowSingle: false, nameProperty: 'mapping' }
	    };
	    this.callParent(arguments);

	}
    });

    // Define a data.model  for the gene table
    Ext.define('geneTableEntry', {
	extend: 'Ext.data.Model',
	fields: [
	    {name: 'genename', type: 'string'},
	    {name: 'dispersion', type: 'number'},
	    {name: 'score', type: 'number'}
	]
    });

    // Define a data.modle for the geneSets table
    Ext.define('geneSetTableEntry', {
	extend: 'Ext.data.Model',
	fields: [
	    {name: 'locked', type: 'boolean'},
	    {name: 'genesetname', type: 'string'},
	    {name: 'shortdescription', type: 'string'}

	]
    });

    Ext.define('aspectTableEntry', {
      extend: 'Ext.data.Model',
      fields: [
        {name: 'name', type: 'string'}
      ]

    });

    Ext.define('genesetInAspectEntry', {
      extend: 'Ext.data.Model',
      fields: [
        {name: 'name', type: 'string'}
      ]
    })

} // dataController.prototype.defineExtJsObjects

/**
 * Get an ExtJS information store configured to get
 * data from a server via ajax calls
 * @private
 */
dataController.prototype.getRemoteGeneInformationStore = function(callback) {
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
}


/**
 * Retrieves gene information for all genes in JSON
 * format
 * @param callback the callback function
 */
dataController.prototype.getGeneInformation =  function(callback) {
  $.ajax({
  	dataType: "json",
  	url: "getData.php",
  	data: {'dataidentifier': 'geneinformation' },
  	success: function(data) {
  	    callback(data);
  	}
  });
}







/**
 * Get part of the aspect matrix specifying both the aspects to get and the cell start/end indices
 * @cellIndexStart start cell index
 * @cellIndexEnd end cell index
 * @aspectIds the ids of the aspect to get
 * @getCellNames logical flag denoting if the cell names are to be received
 * @callback callback function
 */
dataController.prototype.getAspectMatrixByAspect = function(cellIndexStart, cellIndexEnd, aspectIds, callback) {
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
    	"aspectids": aspectIds,
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

	    var dataCntr = new dataController();
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


