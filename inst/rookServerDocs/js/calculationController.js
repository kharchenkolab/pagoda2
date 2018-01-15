"use strict";

/**
 * Perform computations on the data
 * @constructor
 */
function calculationController(localAvailable, remoteAvailable) {
    if ( typeof calculationController.instance === 'object' ){
	    return calculationController.instance;
    };

    this.methods = [
      {
        name: 'localWilcoxon',
        displayName: 'Local Mann-Whitney U test',
        help: 'Tie Corrected Mann-Whitney U test run on the browser locally',
        repos: 'wilcoxon',
      },
      {
        name: 'remoteDefault',
        displayName: 'Remote Default',
        help: 'Remote Default Method',
        repos: 'remote'
      }
    ];
    this.defaultMethod = 'localWilcoxon';

    calculationController.instance = this;
    return this;
}

/**
 * Return the methods that are available for performing DE
 */
calculationController.prototype.getAvailableDEmethods = function() {
  return this.methods;
}

/**
 * Get the current method name
 */
calculationController.prototype.getDefaultMethodName = function(){
  return this.defaultMethod;
}

/**
 * Abort the current processing
 */ 
calculationController.prototype.abort = function() {
  this.localWorker.postMessage({ type: "abort" });
}

/**
 * Calculate differential expression between two cell sets
 * given a specific (local or remote method)
 */
calculationController.prototype.calculateDEfor2selections = function(selectionA, selectionB, method, callback) {
  if (method === 'remoteDefault') {
    return this.calculateDEfor2selectionsbyRemote(selectionA, selectionB, callback);
  } else if(method === 'localWilcoxon'){
    return this.calculateDEfor2selectionsbyLocal(selectionA, selectionB, callback,"wilcoxon");
  }  else {
    callback('Not implemented');
  }
};

/**
 * Calculate differential expression between one cell set and everything else
 * given a specific (local or remote method)
 */
calculationController.prototype.calculateDEfor1selection = function(selectionA, method, callback) {
  if (method === 'remoteDefault') {
    return this.calculateDEfor1selectionbyRemote(selectionA, callback);
  } else if(method=== 'localWilcoxon'){
    var cellSelCntr = new cellSelectionController();
    return this.calculateDELocal([cellSelCntr.getSelection(selectionA)], callback, "wilcoxon");
  } else {
    callback('Not implemented');
  }
};

/**
 * Calculate differential expression for 1 selection against the packground
 * @param selectionA the name of the first cell selection as registered in the cell selection controller
 * @param selectionB the name of the second cell selection as registered in the cell selection controller
 * @param callback
 * @param method the identifier for the local being used to calculate
 */
calculationController.prototype.calculateDEfor2selectionsbyLocal = function(selectionA, selectionB, callback, method){
  var cellSelCntr = new cellSelectionController();
  return this.calculateDELocal([cellSelCntr.getSelection(selectionA), cellSelCntr.getSelection(selectionB)], callback, method);
};

/**
 * Calculate differential expression for a group of selections against the background
 * @param selections An array of the cell selections to be used to perform
 * differential expression analysis as an array of cell names
 * @param callback
 * @param method the identifier for the local being used to calculate
 */
calculationController.prototype.calculateDELocal = function(selections, callback, method){
  var thisController = this;
  var dataCtrl = new dataController();
  var calcCtrl = new calculationController();

  calcCtrl.selections = selections;
  calcCtrl.callback = callback;

  // Generates the new worker
  if(typeof(this.localWorker) === "undefined") {
      this.localWorker = new Worker('js/lightDeWorker.js');
      
        var executeDE =  function() {
            var cellsRetrieve = calcCtrl.selections[0].concat(calcCtrl.selections[1]);
            
            //builds non-modal progress bar window
            (new actionPanelUIcontroller()).showDisplayBar();
            (new actionPanelUIcontroller()).setProgressLabel("Downloading...");
            
            // Run de
            dataCtrl.getExpressionValuesSparseByCellName(
              cellsRetrieve, 
              function(data){
                (new actionPanelUIcontroller()).setProgressLabel("Calculating...");  
                calculationController.instance.localWorker.postMessage({
                  type: "rundiffexpr",
                  data: data, // The sparse array
                  selections: calcCtrl.selections // The selections to know what is compared with what
                });
              },
              function(percent) {
                (new actionPanelUIcontroller()).updateProgressPercent(percent*0.5);
              }
            ); // getExpressionValuesSparseByCellName
              

        }; // executeDE
      
        var selections =  calcCtrl.selections;
        if(selections.length === 1){
          dataCtrl.getCellOrder(function(cellnames){
            selections[1] = [];
            for(var i = 0 ; i < cellnames.length; i++) {
              if(selections[0].indexOf(cellnames[i]) === -1) {
                selections[1].push(cellnames[i]);
              }
            }
            executeDE();
          })
        } else {
           executeDE();
        }
    } // localWorker undefined

  // Handle the incoming message
  this.localWorker.onmessage = thisController.handleWorkerMessage;
}

/**
 * Handle an incoming message from the worker thread
 */
calculationController.prototype.handleWorkerMessage = function(e) {
    var w = this; // In worker context
    var cellSelCntr = new cellSelectionController();
    var callParams = e.data;
    var dataCtrl = new dataController();
    var calcCtrl = new calculationController();

    if(callParams.type === "complete"){

      // This shouldn't be here
      if(document.getElementById("localProgressBar")){
        var calcCtrl = new calculationController();
        calcCtrl.localWorker = undefined;
        (new actionPanelUIcontroller()).setProgressLabel("Finishing...");
        setTimeout(function(){
          calcCtrl.callback(e.data.results);
          Ext.getCmp("localProgressBarWindow").close();
        },0.100);
        
      } 
    } else if (callParams.type === "progressupdate") {
      
      var thisController = new calculationController();
      var totalProgress = 0.5 + (callParams.current / callParams.outoff) * 0.5;
      (new actionPanelUIcontroller()).updateProgressPercent(totalProgress);
      
    } else if (callParams.type === "aborted") {
        w.terminate();
        calcCtrl.localWorker = undefined;
        (new actionPanelUIcontroller()).setProgressLabel("Finishing...");
                setTimeout(function(){
          calcCtrl.callback(e.data.results);
          Ext.getCmp("localProgressBarWindow").close();
        },0.100);
        
    }
}; // handleWorkerMessage

/**
 * Show the display bar window
 */


/// Server backed calculations below

/**
 * Calculate differential expression between two groups of cells via connection to a remote server
 * @param selectionA the name of the first cell selection as registered in the cell selection controller
 */
calculationController.prototype.calculateDEfor1selectionbyRemote = function(selectionA, callback) {
  var cellSelCntr = new cellSelectionController();
  var selAcells = cellSelCntr.getSelection(selectionA);

  // Alot of cell identifiers to send, send by POST
	var ajaxObj = $.ajax({
	    type: "POST",
	    dataType: 'json',
	    data: {
	      "compidentifier": "doDifferentialExpression1selection",
	      "selectionA": Ext.util.JSON.encode(selAcells)
	    },
	    url: "doComputation.php?compidentifier=doDifferentialExpression1selection",
	    success: function(data) {
		    callback(data);
	    }
	});

	return ajaxObj;
}

/**
 * Calculate differential expression between two groups of cells via connection to a remote server
 * @param selectionA the name of the first cell selection as registered in the cell selection controller
 * @param selectionB the name of the second cell selection as registered in the cell selection controller
 */
calculationController.prototype.calculateDEfor2selectionsbyRemote = function(selectionA, selectionB, callback) {
  var cellSelCntr = new cellSelectionController();
  var selAcells = cellSelCntr.getSelection(selectionA);
  var selBcells = cellSelCntr.getSelection(selectionB);
  // Alot of cell identifiers to send, send by POST
	var ajaxObj =  $.ajax({
	    type: "POST",
	    dataType: 'json',
	    data: {
	      "compidentifier": "doDifferentialExpression2selections",
	      "selectionA": Ext.util.JSON.encode(selAcells),
	      "selectionB": Ext.util.JSON.encode(selBcells),
	    },
	    url: "doComputation.php?compidentifier=doDifferentialExpression2selections",
	    success: function(data) {
		    callback(data);
	    }
	});

	return ajaxObj;
}


