
/**
 * Handles data access to pagoda2 files
 * @description provides a dataController interface backed by pagoda2 files
 * The object can connect either to remove or local files using different filereader
 * initialisation, according to the parameters provided by loadParams
 */
function DataControllerFile(loadParams) {
  if(!(loadParams.connectionType == 'remoteFile')) {
    throw new Error('DataControllerFile does not support the selected connection type');
  }

  // TODO: handle local

  // Make a p2file reader with the appropriate settings and save it here
  var frTemp = new p2FileReader('remote', loadParams.remoteFileUrl,null);

  // Generate format reader that will use the p2File Reader to access the data
  // the format reader handles the top level index and resolution to file locations
  // the file reader is responsible for obtaining the data in the requested range
  this.formatReader = new p2FormatReader(frTemp);
  this.formatReader.onReady = function() { console.log('On ready fired'); }
  this.formatReader.readHeaderIndex();

  // object for storing the preloaded information for the sparse array
  // this includes offsets and the p vector
  this.sparseArrayPreloadInfo = null;
}

/**
 * Returns the reduced dendrogram
 */
DataControllerFile.prototype.getReducedDendrogram = function(callback) {
  // FIXME: Assume format reader is ready here
  var fr = this.formatReader;

  var fn = function() {
        fr.getEntryAsText('reduceddendrogram', function(text) {
      		// Find lenght of null terminated string
          var dataLength = DataControllerFile.prototype.getNullTerminatedStringLength(text);
      		var textTrimmed = text.slice(0, dataLength);
      		callback(JSON.parse(textTrimmed));

  	 }, fr);
  }

  // Call immediately or defer to when the object is ready
  if (fr.state == fr.READY) {
    fn();
  } else {
    fr.addEventListener('onready',fn);
  }
}


/**
 * Get the cell order
 */
DataControllerFile.prototype.getCellOrder = function(callback) {
  // FIXME: Assume format reader is ready here
  var fr = this.formatReader;

  var fn = function() {
        fr.getEntryAsText('cellorder', function(text) {
          var dataLength = DataControllerFile.prototype.getNullTerminatedStringLength(text);
      		var textTrimmed = text.slice(0, dataLength);
      		callback(JSON.parse(textTrimmed));

  	 }, fr);
  }

  // Call immediately or defer to when the object is ready
  if (fr.state == fr.READY) {
    fn();
  } else {
    fr.addEventListener('onready',fn);
  }
}


/**
 * Get the cell metadata
 */
DataControllerFile.prototype.getCellMetadata = function(callback) {
  // FIXME: Assume format reader is ready here
  var fr = this.formatReader;

  var fn = function() {
        fr.getEntryAsText('cellmetadata', function(text) {
      		var dataLength = DataControllerFile.prototype.getNullTerminatedStringLength(text);

      		var textTrimmed = text.slice(0, dataLength);
      		callback(JSON.parse(textTrimmed));

  	 }, fr);
  }

  // Call immediately or defer to when the object is ready
  if (fr.state == fr.READY) {
    fn();
  } else {
    fr.addEventListener('onready',fn);
  }

};


/**
 * Get the gene information store
 */
DataControllerFile.prototype.getGeneInformationStore = function(callback) {
  var fr = this.formatReader;

  var fn = function() {
        fr.getEntryAsText('geneinformation', function(text) {

          var dataLength = DataControllerFile.prototype.getNullTerminatedStringLength(text);
      		var textTrimmed = text.slice(0, dataLength);

      		var data = JSON.parse(textTrimmed);

      		var pagingStore = Ext.create('LocalJsonStore', {
        		autoLoad: true,
        		model: 'geneTableEntry',
        		pageSize: 100,
        		localData: data,
    	    });
    	    pagingStore.sort('dispersion', 'DESC');
    	    callback(pagingStore);
  	 }, fr);
  };

  // Call immediately or defer to when the object is ready
  if (fr.state == fr.READY) {
    fn();
  } else {
    fr.addEventListener('onready',fn);
  }
};

/**
 * Get the hierarchy of embeddings and reductions
 * @private
 */
DataControllerFile.prototype.getEmbeddingStructure = function(callback) {
  var fr = this.formatReader;

  var fn = function() {
        fr.getEntryAsText('embeddingstructure', function(text) {
      		var dataLength = DataControllerFile.prototype.getNullTerminatedStringLength(text);

      		var textTrimmed = text.slice(0, dataLength);
      		callback(JSON.parse(textTrimmed));

  	 }, fr);
  }

  // Call immediately or defer to when the object is ready
  if (fr.state == fr.READY) {
    fn();
  } else {
    fr.addEventListener('onready',fn);
  }
};


/**
 * Loads the embedding structure that provides information
 * about the reduction and embedding hierarchy
 */
DataControllerFile.prototype.getAvailableReductionTypes = function(callback) {
  this.getEmbeddingStructure(function(data) {
    var ret = [];
    for(i in data) {
      ret.push(i);
    }
    callback(ret);
  });
};

/**
 * Get the specified embedding
 */
DataControllerFile.prototype.getEmbedding = function(type, embeddingType, callback) {

  var fr = this.formatReader;

  this.getEmbeddingStructure(function(data) {
    var embRecordId = data[type][embeddingType];

    var fn = function() {
          fr.getEntryAsText(embRecordId, function(text) {
        		var dataLength = DataControllerFile.prototype.getNullTerminatedStringLength(text);

        		var textTrimmed = text.slice(0, dataLength);
        		var data = JSON.parse(textTrimmed);

        		// TODO: Check that the arrays contain numbers
        		var unpackedValues = DataControllerServer.prototype.unpackCompressedBase64Float64Array(data.values);
        		data.values = unpackedValues;

    		    callback(data);

    	 }, fr);
    }


    // Call immediately or defer to when the object is ready
    if (fr.state == fr.READY) {
      fn();
    } else {
      fr.addEventListener('onready',fn);
    }
    //callback(ret);
  });
};

/**
 * Get a structure with the information locally required for doing requests to the array
 * @description this function will return an object with the information required to access
 * a serialised sparse array with requests of specific ranges in both dimentions
 * This comprises: (1) The start position of the array entry in thefile in blocks (from the file index)
 * (2) The offsets of each elements wrt the starting positions in bytes (obtained from the sparse matrix index)
 * (3) The dimnames (that are stored as json string) (Dimname1 and Dimname2)
 * (4) The full p array
 */
DataControllerFile.prototype.getSparseArrayPreloadInformation = function(entryName, callback) {
  var fr = this.formatReader;
  var dcf = this;

  // Get the sparse matrix index and cache it. This is 8 uint32_t long (32 bytes)
  fr.getBytesInEntry(entryName, 0, 32, function(data){

    var dataArray = new Uint32Array(data);

    dcf.sparseArrayPreloadInfo = {};
    dcf.sparseArrayPreloadInfo.dim1 = dataArray[0];
    dcf.sparseArrayPreloadInfo.dim2 = dataArray[1];
    dcf.sparseArrayPreloadInfo.pStartOffset = dataArray[2];
    dcf.sparseArrayPreloadInfo.iStartOffset = dataArray[3];
    dcf.sparseArrayPreloadInfo.xStartOffset = dataArray[4];
    dcf.sparseArrayPreloadInfo.dimname1StartOffset = dataArray[5];
    dcf.sparseArrayPreloadInfo.dimname2StartOffset = dataArray[6];
    dcf.sparseArrayPreloadInfo.dimnames2EndOffset  = dataArray[7];

    dcf.sparseArrayPreloadInfo.dimnames1Data = null;
    dcf.sparseArrayPreloadInfo.dimnames2Data = null;
    dcf.sparseArrayPreloadInfo.parray = null;

    var callCallbackIfReady = function() {
      var ready = false;
      if (dcf.sparseArrayPreloadInfo.dimnames1Data !== null &&
       dcf.sparseArrayPreloadInfo.dimnames2Data !== null &&
       dcf.sparseArrayPreloadInfo.parray !== null) {
         ready = true;
       }

      if (ready) {
        callback();
      }
    }

    var parraylength =  dcf.sparseArrayPreloadInfo.iStartOffset -  dcf.sparseArrayPreloadInfo.pStartOffset;
    fr.getBytesInEntry(entryName, dcf.sparseArrayPreloadInfo.pStartOffset, parraylength, function(buffer){
        dcf.sparseArrayPreloadInfo.parray = new Uint32Array(buffer);
        callCallbackIfReady();

    }, fr);

    var dimnames1length = dcf.sparseArrayPreloadInfo.dimname2StartOffset - dcf.sparseArrayPreloadInfo.dimname1StartOffset - 1; // -1 for null
    fr.getBytesInEntryAsText(entryName, dcf.sparseArrayPreloadInfo.dimname1StartOffset, dimnames1length,
      function(data){
        dcf.sparseArrayPreloadInfo.dimnames1Data = JSON.parse(data);

        // Build reverse map
        dcf.sparseArrayPreloadInfo.dimnames1DataReverse = {};
        for (var i in dcf.sparseArrayPreloadInfo.dimnames1Data) {
          dcf.sparseArrayPreloadInfo.dimnames1DataReverse[dcf.sparseArrayPreloadInfo.dimnames1Data[i]] = parseInt(i);
        }

        callCallbackIfReady();
    },fr);

    var dimnames2length = dcf.sparseArrayPreloadInfo.dimnames2EndOffset - dcf.sparseArrayPreloadInfo.dimname2StartOffset - 1; // -1 for null
    fr.getBytesInEntryAsText(entryName, dcf.sparseArrayPreloadInfo.dimname2StartOffset, dimnames2length,
      function(data){
        dcf.sparseArrayPreloadInfo.dimnames2Data = JSON.parse(data);

        // Build reverse map
        dcf.sparseArrayPreloadInfo.dimnames2DataReverse = {};
        for (var i in dcf.sparseArrayPreloadInfo.dimnames2Data) {
          dcf.sparseArrayPreloadInfo.dimnames2DataReverse[dcf.sparseArrayPreloadInfo.dimnames2Data[i]] = parseInt(i);
        }

        callCallbackIfReady();
    },fr);

  }, fr);
};

/**
 * Get a single gene column from the file sparse matrix
 * @private
 */
DataControllerFile.prototype.getGeneColumn = function(geneName, geneindex, cellIndexStart, cellIndexEnd, callback) {
  var dcf = this;
  var fr = this.formatReader;

  // Index of the gene
  var geneIndexInSparse = dcf.sparseArrayPreloadInfo.dimnames2DataReverse[geneName];

  // Column start and end index
  var csi = dcf.sparseArrayPreloadInfo.parray[geneIndexInSparse] - 1;
  var cei = dcf.sparseArrayPreloadInfo.parray[geneIndexInSparse + 1]  - 1;

  // Zero filled array with the data for all the cells
  var fullRowArray = new Float32Array(dcf.sparseArrayPreloadInfo.dim1);

  // Get csi to cei for the x array
  const BYTES_PER_FLOAT32 = 4;
  var xArrayOffset = dcf.sparseArrayPreloadInfo.xStartOffset + BYTES_PER_FLOAT32;

  // Byte position in the file that corresponds to the csi and cei indexes
  var csiBytes = xArrayOffset + csi * BYTES_PER_FLOAT32;
  var ceiBytes = xArrayOffset + cei * BYTES_PER_FLOAT32;

  // Get the number of bytes to retrieve
  var xRowLength = ceiBytes - csiBytes;

  // Get the x array bytes (the raw information)
  fr.getBytesInEntry('sparseMatrix', csiBytes, xRowLength, function(buffer) {
    var geneIndexInRequest = geneIndexInRequest;
    var rowXArray = new Float32Array(buffer);

    // Calculate positions of i array entries in the file
    var iArrayOffset = dcf.sparseArrayPreloadInfo.iStartOffset + BYTES_PER_FLOAT32;
    var csiBytesI = iArrayOffset + csi * BYTES_PER_FLOAT32;
    var ceiBytesI = iArrayOffset + cei * BYTES_PER_FLOAT32;
    var xRowLengthI = ceiBytes - csiBytes;

    // Get the p array bytes
    fr.getBytesInEntry('sparseMatrix', csiBytesI, xRowLengthI, function(buffer2) {
      var rowIArray = new Uint32Array(buffer2);

      // Expand the array to a full array
      for (k =0; k < rowIArray.length; k++) {
        var ki = rowIArray[k];
        fullRowArray[ki] = rowXArray[k];
      }

      // Slice to only get requested entries
      // TODO: This can be optimised so that only values that are needed to begin with
      // are stored. This can be done with an if in the inner loop, that check the ki index
      // for being in range and offsetting as required
      var retVal = fullRowArray.slice(cellIndexStart,cellIndexEnd);

      // Done, do the callback
      callback(geneName, geneindex, retVal);
    });
  });
}

/**
 * Performs the getExpressionValuesSparseByCellIndexUnpacked
 * @description this is to be called by the object only as it assumes that
 * the correct initialisation is done
 * @private
 */
DataControllerFile.prototype.getExpressionValuesSparseByCellIndexUnpackedInternal =
  function(geneIds, cellIndexStart, cellIndexEnd, getCellNames, callback){

  var dcf = this;
  var fr = this.formatReader;

  // Array to track progress of row generation with the async calls
  // Each request corresponds to the gene index
  var progressArray = new Uint32Array(geneIds.length);

  // The array to store the results as they come back
  var resultsArray = [];

  // Check if all tasks are done and call backback if so
  function checkIfDone(callback) {
    var done = true;
    for(var i = 0; i < progressArray.length; i++) {
      if (progressArray[i] !==  1) {
        done = false;
        break;
      }
    }
    if (done) {
      if(typeof callback === 'function') {
        callback();
      }
    }
  }

  // Runs when all the data is available
  function handleComplete(callback, dcf, cellIndexStart, cellIndexEnd) {
        // Convert to sparse matrix and return to callback
        var x = new Array();
        var i = new Array();
        var p = new Array()

        var dim1Length = resultsArray.length;
        var dim2Length = resultsArray[0].length;

        // Pack the subsetted array back into a sparse array
        // That the downstream functions expect
        var pos = 0;
        for (k = 0; k < dim1Length; k++) {
          p.push(pos); // Start of the column
          for (j =0; j < dim2Length; j++) {
              if (resultsArray[k][j] != 0) { // TODO: perhaps 1e-16
                x.push(resultsArray[k][j]); // The value
                i.push(j); // corresponding index j or K?
                pos++;
              }
          }
        }
        p.push(pos); // p push number of elements

        // TODO: consider converting the arrays to typed

        var cellNames = dcf.sparseArrayPreloadInfo.dimnames1Data.slice(cellIndexStart, cellIndexEnd);
        var retVal = new dgCMatrixReader(i, p , [dim2Length,dim1Length], cellNames, geneIds, x, null);

        // Done, do callback
        callback(retVal);
  }

  // Initiate callbacks for each row
  for(var geneIndexInRequest in geneIds) {
    var geneName = geneIds[geneIndexInRequest];
    dcf.getGeneColumn(geneName, geneIndexInRequest, cellIndexStart, cellIndexEnd, function(genename, geneindex, row) {
      progressArray[geneindex] = 1;
      resultsArray[geneindex] = row;
      checkIfDone(function(){
        handleComplete(callback, dcf, cellIndexStart, cellIndexEnd);
      }); // checkIfDone
    }); // getGeneColumn
  } // for each gene

} // getExpressionValuesSparseByCellIndexUnpackedInternal

/**
 * Gets the expression values sparse
 *
 */
DataControllerFile.prototype.getExpressionValuesSparseByCellIndexUnpacked =
  function(geneIds, cellIndexStart, cellIndexEnd, getCellNames, callback){

    var dcf = this;

    if(this.sparseArrayPreloadInfo === null) {
      // Need to preload
      var dcf = this;
      this.getSparseArrayPreloadInformation('sparseMatrix',function() {
        dcf.getExpressionValuesSparseByCellIndexUnpackedInternal(geneIds, cellIndexStart, cellIndexEnd, getCellNames, callback);
      })
    } else {
      dcf.getExpressionValuesSparseByCellIndexUnpackedInternal(geneIds, cellIndexStart, cellIndexEnd, getCellNames, callback);
    }

}

/**
 * Helper function that returns the length of a null terminated string
 */
DataControllerFile.prototype.getNullTerminatedStringLength = function(text) {
  		var dataLength = 0;
  		var textLength = text.length;
  		for (; dataLength < textLength; dataLength++) {
  		    if (text.charCodeAt(dataLength) === 0) {
  			    break;
  		    }
  		}
  		return dataLength;
}

/**
 * Get the available embeddings for a reduction
 */
DataControllerFile.prototype.getAvailableEmbeddings = function(type, callback, callbackParams) {
  this.getEmbeddingStructure(function(data) {
    var ret = [];
    for(i in data[type]) {
      ret.push(i);
    }
    callback(ret, callbackParams);
  });
};



// Unimplemented
DataControllerFile.prototype.getGeneSetInformationStore = function(callback) {

}

DataControllerFile.prototype.getAspectMatrixByAspect = function(cellIndexStart, cellIndexEnd, aspectIds, callback) {

}

DataControllerFile.prototype.getGeneSetStoreByName = function(name, callback) {

}


DataControllerFile.prototype.getAvailableAspectsStore = function(callback) {

}

DataControllerFile.prototype.getAvailableGenesetsInAspectsStore = function(aspectId, callback) {

}

DataControllerFile.prototype.getAspectMatrix = function(cellIndexStart, cellIndexEnd, getCellNames, callback) {

}
