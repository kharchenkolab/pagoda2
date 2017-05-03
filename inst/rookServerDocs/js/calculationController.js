/**
 * Perform compulations on the data
 * @constructor
 */
function calculationController(localAvailable, remoteAvailable) {
    if ( typeof calculationController.instance === 'object' ){
	    return calculationController.instance;
    };

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


