/**
 * Perform compulations on the data
 * @constructor
 */
function calculationController(localAvailable, remoteAvailable) {
    if ( typeof calculationController.instance === 'object' ){
	    return calculationController.instance;
    };

    // TODO: Don't hard-code these
    this.methods = [
      {
        name: 'remoteDefault',
        displayName: 'Remote Default',
        help: 'Remote Default Method',
        repos: 'remote'
      },
      {
        name: 'localDefault',
        displayName: 'Local Default',
        help: 'Local default method',
        repos: 'T-test'
      }
    ];

    calculationController.instance = this;
    return this;
}

/**
 * Calculate differential expression between two cell sets
 * given a specific (local or remote method)
 */
calculationController.prototype.calculateDEbySelection = function(selectionA, selectionB, method, callback) {
  if (method === 'remoteDefault') {
    this.calculateDEbySelectionRemote(selectionA, selectionB, callback);
  } else {
    callback('Not implemented');
  }
}

calculationController.prototype.calculateDEbySelectionRemote = function(selectionA, selectionB, callback) {
  var cellSelCntr = new cellSelectionController();
  selAcells = cellSelCntr.getSelection(selectionA);
  selBcells = cellSelCntr.getSelection(selectionB);

  // Alot of cell identifiers to send, send by POST
	$.ajax({
	    type: "POST",
	    dataType: 'json',
	    data: {
	      "compidentifier": "doDifferentialExpression",
	      "selectionA": Ext.util.JSON.encode(selAcells),
	      "selectionB": Ext.util.JSON.encode(selBcells),
	    },
	    url: "doComputation.php?compidentifier=doDifferentialExpression",
	    success: function(data) {
		    callback(data);
	    }
	});

}

/**
 * Return the methods that are available for performing DE
 */
calculationController.prototype.getAvailableDEmethods = function() {
  return this.methods;
}


/**
 * Stores differential expression results and accompanying metadata
 * Singleton
 * @constructor
 */
function diffExpressionStore() {
    if ( typeof differentialExpressionStorageController.instance === 'object' ){
	    return differentialExpressionStorageController.instance;
    };

    this.deSets = new Array();

    differentialExpressionStorageController.instance = this;
    return this;
};

diffExpressionStore.prototype.getAvailableDEsets = function() {

};

diffExpressionStore.prototype.addDEset = function(deset) {
  this.deSets.push(deset);
};

/**
 * Represents a differential expression result set and accompanying metadata
 */
function deResultSet() {
  this.selectionA = null;
  this.selectionB = null;
  this.results = null;
  this.name = null;
};

deResultSet.prototype.getName = function() {
  return this.name;
};

deResultSet.prototype.setName = function(val) {
  this.name = val;
};

deResultSet.prototype.getSelectionA = function() {
  return this.selectionA;
};

deResultSet.prototype.setSelectionA = function(val) {
  this.seletionA = val;
};

deResultSet.prototype.getSelectionB = function() {
  return this.selectionB;
};

deResultSet.prototype.setSelectionB = function(val) {
  this.selectionB = val;
};




