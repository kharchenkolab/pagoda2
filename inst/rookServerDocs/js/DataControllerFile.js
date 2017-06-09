
/**
 * Handles data access by File, either local or remote
 */
function DataControllerFile(loadParams) {
  if(!(loadParams.connectionType == 'remoteFile')) {
    throw new Error('DataControllerFile does not support the selected connection type');
  }

  // TODO: handle local

  // Make a p2file reader with the appropriate settings and save it here
  var frTemp = new p2FileReader('remote', loadParams.remoteFileUrl,null);

  // Generate format reader
  this.formatReader = new p2FormatReader(frTemp);
  this.formatReader.onReady = function() { console.log('On ready fired'); }
  this.formatReader.readHeaderIndex();

  this.sparseArrayPreloadInfo = null;

  this.embeddingStructure = null;

}


DataControllerFile.prototype.getReducedDendrogram = function(callback) {
  // Assume format reader is ready here
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

DataControllerFile.prototype.getCellOrder = function(callback) {
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



DataControllerFile.prototype.getCellMetadata = function(callback) {
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
  }


  // Call immediately or defer to when the object is ready
  if (fr.state == fr.READY) {
    fn();
  } else {
    fr.addEventListener('onready',fn);
  }
};

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

DataControllerFile.prototype.getAvailableEmbeddings = function(type, callback, callbackParams) {
  this.getEmbeddingStructure(function(data) {
    var ret = [];
    for(i in data[type]) {
      ret.push(i);
    }
    callback(ret, callbackParams);
  });
};

DataControllerFile.prototype.getGeneSetInformationStore = function(callback) {

}

DataControllerFile.prototype.getAspectMatrixByAspect = function(cellIndexStart, cellIndexEnd, aspectIds, callback) {

}

DataControllerFile.prototype.getGeneSetStoreByName = function(name, callback) {

}

DataControllerFile.prototype.getExpressionValuesSparseByCellIndexUnpacked =
  function(geneIds, cellIndexStart, cellIndexEnd, getCellNames, callback) {



}

DataControllerFile.prototype.getAvailableAspectsStore = function(callback) {

}

DataControllerFile.prototype.getAvailableGenesetsInAspectsStore = function(aspectId, callback) {

}

DataControllerFile.prototype.getAspectMatrix = function(cellIndexStart, cellIndexEnd, getCellNames, callback) {

}

//DataControllerFile.prototype.getExpressionValuesByCellIndexUnpacked = function(geneIds, cellIndexStart, cellIndexEnd,getCellNames, callback) {}

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

DataControllerFile.prototype.getExpressionValuesSparseByCellIndexUnpackedInternal =
  function(geneIds, cellIndexStart, cellIndexEnd, getCellNames, callback){

  var dcf = this;
  var fr = this.formatReader;

  for(var geneIndexInRequest in geneIds) {
    var geneName = geneIds[geneIndexInRequest];
    console.log('geneName: ', geneName);

    var geneIndexInSparse = dcf.sparseArrayPreloadInfo.dimnames2DataReverse[geneName];
    console.log('geneIndexInSparse: ', geneIndexInSparse);

    var csi = dcf.sparseArrayPreloadInfo.parray[geneIndexInSparse]; // CHECK: is -/+ required
    var cei = dcf.sparseArrayPreloadInfo.parray[geneIndexInSparse + 1];
    console.log('cse/cei:', csi, cei);

    // Zero filled array with the data for all the cells
    var fullRowArray = new Uint32Array(dcf.sparseArrayPreloadInfo.dim1);

    // Get csi to cei for the x array
    var xArrayOffset = dcf.sparseArrayPreloadInfo.xStartOffset;
    var csiBytes = xArrayOffset + csi * 4; // 4 bytes per float
    var ceiBytes = xArrayOffset + cei * 4;
    var xRowLength = ceiBytes - csiBytes;
    console.log("csiBytes:", csiBytes);
    console.log("xRowLength:",xRowLength);


    fr.getBytesInEntry('sparseMatrix', csiBytes, xRowLength, function(buffer) {
      var rowXArray = new Float32Array(buffer);

      var iArrayOffset = dcf.sparseArrayPreloadInfo.iStartOffset;
      var csiBytesI = iArrayOffset + csi * 4; // 4 bytes per float
      var ceiBytesI = iArrayOffset + cei * 4;
      var xRowLengthI = ceiBytes - csiBytes;

      fr.getBytesInEntry('sparseMatrix', csiBytesI, xRowLengthI, function(buffer2) {
        var rowIArray = new Uint32Array(buffer2);
        console.log(rowIArray);

        for (k in rowIArray) {
          fullRowArray[k] = rowXArray[k];
        }

        console.log(fullRowArray);
      });


      console.log(rowXArray);
    });



    // TODO:
    // Get corresponding slice of the i and x arrays
    // Initialise an empty array filled with 0s and put the values in it
    // push the whole array onto a main array to be returned
    // confirm t() is not required




    // We will need  to return a new dgCMatrixReader,so I should be building an array for this
    // or write a method to do make a dgCMatrixReader from a full array (better)

}
}


DataControllerFile.prototype.getExpressionValuesSparseByCellIndexUnpacked =
  function(geneIds, cellIndexStart, cellIndexEnd, getCellNames, callback){

  var dcf = this;

  if(this.sparseArrayPreloadInfo === null) {
    // Need to preload
    var dcf = this;
    this.getSparseArrayPreloadInformation('sparseMatrix',function() {
      console.log('In the callback:', dcf.sparseArrayPreloadInfo);
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
