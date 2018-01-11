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
        name: 'remoteDefault',
        displayName: 'Remote Default',
        help: 'Remote Default Method',
        repos: 'remote'
      },
      {
        name: 'localWilcoxon',
        displayName: 'Local Mann-Whitney U test',
        help: 'Tie Corrected Mann-Whitney U test run of the browser locally',
        repos: 'wilcoxon',
      }
    ];

    calculationController.instance = this;
    return this;
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
            dataCtrl.getExpressionValuesSparseByCellName(cellsRetrieve, function(data){
              thisController.setProgressLabel("Calculating...");  
              calculationController.instance.localWorker.postMessage({
                type: "rundiffexpr",
                data: data, // The sparse array
                selections: calcCtrl.selections // The selections to know what is compared with what
              });
            },function(percent) {
              thisController.updateProgressPercent(percent*0.5);
            }); // getExpressionValuesSparseByCellName
            //builds non-modal progress bar window
            thisController.showDisplayBar();
            thisController.setProgressLabel("Downloading...");
        }
      
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

calculationController.prototype.terminateWorker = function() {
  if(Ext.getCmp("localProgressBarWindow")){
    Ext.getCmp("localProgressBarWindow").close()
  }

  var calcCtrl = new calculationController();
  calcCtrl.localWorker.terminate();
  calcCtrl.localWorker = undefined;
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
            actionUI.stopAnalysisClickHandler() // FIXME
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
    var calcCtrl = new calculationController();

    if(callParams.type === "complete"){

      // FIXME
      if(document.getElementById("localProgressBar")){
        w.terminate();

        var calcCtrl = new calculationController();
        calcCtrl.localWorker = undefined;

        calcCtrl.setProgressLabel("Finishing...");

        setTimeout(function(){
          calcCtrl.callback(e.data.results);
          Ext.getCmp("localProgressBarWindow").close();
        },0.100);
      } 
    } else if (callParams.type === "progressupdate") {
       var thisController = new calculationController();
        var totalProgress = 0.5+(callParams.current / callParams.outoff)*0.5
        thisController.updateProgressPercent(totalProgress);
      }
} // handleWorkerMessage

calculationController.prototype.setProgressLabel = function(text) {
  document.getElementById("localProgressLabel").innerHTML = text;
}

calculationController.prototype.updateProgressPercent = function(val) {
  var execution = val * 100;
  document.getElementById("localProgressBar").style.width = execution + "%"
  document.getElementById("localProgressLabel").innerHTML = Math.floor(execution*10)/10 + "%";
}

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
