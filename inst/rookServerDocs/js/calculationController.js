"use strict";

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
        name: 'localWilcoxon',
        displayName: 'Local Wilcoxon',
        help: 'Local Wilcoxon Method',
        repos: 'wilcoxon',
      }
    ];

    calculationController.instance = this;
    return this;
}

/**
 * Calculate differential expression for 1 selection against the packground
 * @param selectionA the name of the first cell selection as registered in the cell selection controller
 * @param callback
 * @param method the identifier for the local being used to calculate
 */
calculationController.prototype.calculateDEfor1selectionbyLocal = function(selectionA,callback, method){
  var cellSelCntr = new cellSelectionController();
  return this.calculateDELocal([cellSelCntr.getSelection(selectionA)], callback, method);
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
  } else if(method=== 'localDefault'){
    return this.calculateDEfor1selectionbyLocal(selectionA, callback, "wilcoxon");
  } else {
    callback('Not implemented');
  }
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
    this.localWorker = new Worker("js/statisticsWorker.js");
    dataCtrl.getAllGeneNames(function(geneNames) {

      // Send worker the startup command
      var startUpPackage = {
        command:{
          type: "setup",
        },
        params:{
          method: method,
          geneNames: geneNames,
          results: [],
          closed: false
        }
      };

      thisController.localWorker.postMessage(startUpPackage);

      //builds non-modal progress bar window
      thisController.showDisplayBar();
    });
  }

  // Handle the incoming message
  this.localWorker.onmessage = thisController.handleWorkerMessage;

  return {
    //abort function for stop button functionality
    abort: function(){
      var calcCtrl = new calculationController()
      calcCtrl.localWorker.postMessage({command:{type:"stop"}})
    } // function abort
  }; // return
}

/**
 * Show the display bar window
 */
calculationController.prototype.showDisplayBar = function() {
  Ext.create("Ext.window.Window", {
      title: "Processing Data Locally",
      internalPadding: '10 10 10 10',
      width: "300px",
      id: "localProgressBarWindow",
      resizeable: false,
      items: [
        {
          html:'<div style="width:100%;background-color:#DDDDDD;height:30px"> <div id="localProgressBar" style="width:0%;background-color:#B0E2FF;height:30px; text-align: center;vertical-align: middle;line-height: 30px;"><div id="localProgressLabel" style="float: left; width: 100%; height: 100%; position: absolute; vertical-align: middle;">0%</div></div></div>'
        }
      ],
      listeners:{
        close: function(win){
          var actionUI = new actionPanelUIcontroller();
          if(actionUI.currentDErequest){
            actionUI.stopAnalysisClickHandler()
          }
        },
      }
    }).show(0);
}




/**
 * Handle an incoming message from the worker thread
 */
calculationController.prototype.handleWorkerMessage = function(e) {
    var w = this; // In worker context
    var cellSelCntr = new cellSelectionController();
    var callParams = e.data;
    var dataCtrl = new dataController();
    var calcCntr = new calculationController();

    //in the event of the cell order request sends cell order back with cell selection names
    if(callParams.request.type === "cell order") {
      dataCtrl.getCellOrder(function(cellData){
        w.postMessage({
          command:{
            type: "initiate",
            data: cellData,
            selections: calcCntr.selections,
          },
          params: callParams.params
        })
      });
    } else if(callParams.request.type === "expr vals"){
      //debugger;
      //in the event of a expr vals request sends expression values back for a given chunk of gene names
      if(document.getElementById("localProgressBar")){

        // Update the progress bar
        var execution = (callParams.params.index/callParams.params.geneNames.length) * 100;
        document.getElementById("localProgressBar").style.width = execution + "%"
        document.getElementById("localProgressLabel").innerHTML = Math.floor(execution*10)/10 + "%";

        // Send the next chunk of data
        dataCtrl.getExpressionValuesSparseByCellIndex(callParams.request.data, 0,
          callParams.params.numCells, function(data){
          w.postMessage({
            command:{
              type: "process",
              data: data
            },
            params: callParams.params
          })
        })

      } // if(document.getElementById("localProgressBar")
    } else if(callParams.request.type === "clean death"){
      var calcCtrl = new calculationController();

      //during completion of execution a clean death is evoked
      //if the clean death occures while an abrupt death is being evoked this if should prevent data race
      if(document.getElementById("localProgressBar")){
        // TODO: This should be independent really
        w.terminate();

        var calcCtrl = new calculationController();
        calcCtrl.localWorker = undefined;

        document.getElementById("localProgressLabel").innerHTML = "Finishing"
        setTimeout(function(){
          calcCtrl.callback(callParams.params.results);
          Ext.getCmp("localProgressBarWindow").close();
        },1);
      }
    } else if(callParams.request.type === "abrupt death"){
      //if stop or another button like it is clicked cleanly shutsdown the worker without calling the call back
      if(Ext.getCmp("localProgressBarWindow")){
        Ext.getCmp("localProgressBarWindow").close()
      }
      w.terminate();
      var calcCtrl = new calculationController();
      calcCtrl.localWorker = undefined;
    }
} // handleWorkerMessage























// Remote AJAX Stuff
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

/**
 * Return the methods that are available for performing DE
 */
calculationController.prototype.getAvailableDEmethods = function() {
  return this.methods;
}
