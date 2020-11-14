/* Don't seem to be able to use strict here because of ExtJS */

/*
 * Filename: dataController.js
 * Author: Nikolas Barkas
 * Date: June 2017
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
function dataController(loadParams) {
    if (typeof dataController.instance === 'object') {
    	return dataController.instance;
    };

    // Define global extJS objects (datatype etc)
    this.defineExtJsObjects();

    // Initialise the controller that will serve the data
    this.internalController = null;
    this.cellNameToOrderMap = null;

    if (loadParams.connectionType == 'remoteServer') {
      // Standard Server Backed
      this.internalController = new DataControllerServer();
    } else if (loadParams.connectionType == 'localFile') {
      // Local file with user drag drop
      this.internalController = new DataControllerFile(loadParams);
    } else if (loadParams.connectionType == 'remoteFile') {
      // Remote file url
      this.internalController = new DataControllerFile(loadParams);
    } else {
      throw new Error('Unknown connectionType')
    }

    // Sigleton
    dataController.instance = this;
};

/**
 * Define extJS models and objects required by the data control;
ler.
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
        {name: 'name', type: 'string'},
        {name: 'n', type: 'number'},
        {name: 'cz', type: 'number'},
        {name: 'shortdescription', type: 'string'}
      ]
    })

} // dataController.prototype.defineExtJsObjects


/*
 * Wrapper functions that must be implemented by individual controllers follow
 */

/**
 * Get the reduced dendrogram and the number  of cells in each group (required for plotting)
 * @param callback the callback function to call when you have the data
 */
dataController.prototype.getReducedDendrogram = function(callback) {
   return this.internalController.getReducedDendrogram(callback);
}

/**
 * Get the cell identifiers in the default order
 */
dataController.prototype.getCellOrder = function(callback) {
  
  return this.internalController.getCellOrder(callback);
}

/**
 * Get an extjs store with
 * the available aspects
 */
dataController.prototype.getAvailableAspectsStore = function(callback) {
  return this.internalController.getAvailableAspectsStore(callback);
}

/**
 * Get the available genesets in the selected aspect
 * @return ajax request
 */
dataController.prototype.getAvailableGenesetsInAspectStore = function(aspectId, callback) {
  return this.internalController.getAvailableGenesetsInAspectStore(aspectId, callback);
}

/**
 * Get a dgCMatrixReader with the cell aspect weights for the specified
 * cell index range and all aspects
 */
dataController.prototype.getAspectMatrix = function(cellIndexStart, cellIndexEnd, getCellNames, callback) {
  return this.internalController.getAspectMatrix(cellIndexStart, cellIndexEnd, getCellNames, callback);
}

/**
 * Get expression values for the specified cells
 * @description this function will return all the expression values for the specified cells
 * this is to support fast differential expression analysis. Unlike the getExpressionValuesSparseByCellIndexUnpacked()
 * function this will use a cell-ordered sparse matrix if available to fetch the requested values
 * @param cellNames the cells to get the expression values for
 * @param callback callback function
 */
dataController.prototype.getExpressionValuesSparseByCellName = function(cellNames, callback, progressCallback) {
  return this.internalController.getExpressionValuesSparseByCellName(cellNames, callback, progressCallback);
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
dataController.prototype.getExpressionValuesSparseByCellIndexUnpacked = function(geneIds, cellIndexStart, cellIndexEnd,
                                                                                 getCellNames, callback) {
      return this.internalController.getExpressionValuesSparseByCellIndexUnpacked(geneIds, cellIndexStart, cellIndexEnd, true, callback);
}

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
    return this.internalController.getExpressionValuesSparseByCellIndexUnpacked(geneIds, cellIndexStart, cellIndexEnd, true,
      function(data) {
        callback(data.getFullMatrix());
      }
    );
}

/**
 * Get the available embeddings for the data reduction type
 * @param type the reduction type for which to get embeddings
 * @param callback the callback to call
 * @param callbackParamers parameters to pass to the callback
 */
dataController.prototype.getAvailableEmbeddings =  function(type, callback, callbackParams ) {
  return this.internalController.getAvailableEmbeddings(type, callback, callbackParams);
};

/**
 * Get an embedding for a specific reduction type
 * @param type the reduction type
 * @param embeddingType the embedding type to get
 * @param callback the callback to get
 */
dataController.prototype.getEmbedding = function(type, embeddingType, callback) {
  return this.internalController.getEmbedding(type, embeddingType, callback);
};

/**
 * Get the hierarchy of embeddings and reductions
 */
dataController.prototype.getEmbeddingStructure = function(callback) {
  return this.internalController.getEmbeddingStructure(callback);
};

/**
 * Get the available reduction types
 * @param the callback
 */
dataController.prototype.getAvailableReductionTypes = function(callback) {
  return this.internalController.getAvailableReductionTypes(callback);
};

/**
 * Get the cell metadata
 * @param callback callback funciton
 * @param callbackParameters data to pass to the callback function
 */
dataController.prototype.getCellMetadata = function(callback, callbackParameters) {
   return this.internalController.getCellMetadata(callback, callbackParameters);
};

/**
 * Get an Extjs store for a list of genes that
 * belong to the specified geneset
 * @param name the name of the geneset the list of which to retrieve
 */
dataController.prototype.getGeneSetStoreByName = function(name, callback) {
  return this.internalController.getGeneSetStoreByName(name, callback);
}

/**
 * Get an ExtJS proxy object for connecting to the GeneTable.
 * @description return a extjs store object with the gene table
 * information data
 */
dataController.prototype.getGeneInformationStore = function(callback) {
  return this.internalController.getGeneInformationStore(callback);
};


dataController.prototype.getGeneSetInformationStore = function(callback) {
  return this.internalController.getGeneSetInformationStore(callback);
};

/**
 * Get all the genes names in an array
 */
dataController.prototype.getAllGeneNames = function(callback) {
  this.internalController.getGeneInformationStore(function(geneNameData){
      // TODO: consider caching thi
      var geneNames = [];

      //collects all gene names being analyzed
      for(var i = 0; i < geneNameData.localData.length; i++){
        geneNames.push(geneNameData.localData[i].genename);
      }
      geneNameData = undefined;

      callback(geneNames);
  });
}

/**
 * Get part of the aspect matrix specifying both the aspects to get and the cell start/end indices
 * @param cellIndexStart start cell index
 * @param cellIndexEnd end cell index
 * @param aspectIds the ids of the aspect to get
 * @param getCellNames logical flag denoting if the cell names are to be received
 * @param callback callback function
 */
dataController.prototype.getAspectMatrixByAspect = function(cellIndexStart, cellIndexEnd, aspectIds, callback) {
  return this.internalController.getAspectMatrixByAspect(cellIndexStart, cellIndexEnd, aspectIds, callback);
}


/**
 * Returns a hash from cell names to cell position in the cell order
 * @description returns via callback a hast from cell name to cell position
 */
dataController.prototype.getCellOrderHash = function(callback) {
  var dc = this;

  this.internalController.getCellOrder(function(data){

    if (dc.cellNameToOrderMap === null) {
      dc.cellNameToOrderMap = {};
      for (var i = 0; i < data.length; i++) {
        dc.cellNameToOrderMap[data[i]] = i;
      }
    };
    callback(dc.cellNameToOrderMap);
  });
}

/**
 * Retuns the neighbour genes of the selected query genes
 * @param queryGenes the genes to search for
 * @param callback function to return the results
 */
dataController.prototype.getGeneNeighbours = function(queryGenes, callback) {
  var dc = this;

 // TODO: Currently only implemented for server backend
  this.internalController.getGeneNeighbours(queryGenes, callback);
}

/**
 * Retuns the application metadata for things like application title
 * @param callback function to return the results
 */
dataController.prototype.getAppMetadata = function(callback) {
 var dc = this;
 this.internalController.getAppMetadata(callback);
}

