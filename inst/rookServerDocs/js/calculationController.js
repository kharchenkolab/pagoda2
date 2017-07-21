"use strict";

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
        help: 'Local Default Method',
        repos: 'KStest'
      },
      {
        name: 'localWilcoxon',
        displayName: 'Local Wilcoxon',
        help: 'Local Wilcoxon Method',
        repos: 'wilcoxon',
      },
      {
        name: 'localTtest',
        displayName: 'Local Ttest',
        help: 'Local T-test Method',
        repos: 't_test',
      }
    ];

    this.localWorker;
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
  } else if(method=== 'localDefault'){
    return this.calculateDEfor2selectionsbyLocal(selectionA, selectionB, callback,"default");
  } else if(method === 'localWilcoxon'){
    return this.calculateDEfor2selectionsbyLocal(selectionA, selectionB, callback,"wilcoxon");
  } else if(method === 'localTtest'){
    return this.calculateDEfor2selectionsbyLocal(selectionA, selectionB, callback,"tTest");
  } else {
    callback('Not implemented');
  }
}


/**
 * Calculate differential expression between one cell set and everything else
 * given a specific (local or remote method)
 */
calculationController.prototype.calculateDEfor1selection = function(selectionA, method, callback) {
  if (method === 'remoteDefault') {
    return this.calculateDEfor1selectionbyRemote(selectionA, callback);
  } else if(method=== 'localDefault'){
    return this.calculateDEfor1selectionbyLocal(selectionA, callback, "default");
  } else if(method=== 'localWilcoxon'){
    return this.calculateDEfor1selectionbyLocal(selectionA, callback, "wilcoxon");
  } else if(method === 'localTtest'){
    return this.calculateDEfor1selectionbyLocal(selectionA, callback, "tTest");
  } else {
    callback('Not implemented');
  }
}

calculationController.prototype.calculateDELocal = function(selections, callback, method){
  var thisController = this;
  var dataCtrl = new dataController();
  if(typeof(this.localWorker) === "undefined") {
    this.localWorker = new Worker("js/statisticsWorker.js");
    dataCtrl.getGeneInformationStore(function(geneNameData){
      var geneNames = [];
      for(var i = 0; i < geneNameData.localData.length; i++){
        geneNames.push(geneNameData.localData[i].genename);
      }
      geneNameData = undefined;

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
      }
      thisController.localWorker.postMessage(startUpPackage)
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
    });
  }

  var w = this.localWorker;
  var cellSelCntr = new cellSelectionController();

  this.localWorker.onmessage = function(e){
    var callParams = e.data;

    if(callParams.request.type === "cell order"){
      dataCtrl.getCellOrder(function(cellData){
        w.postMessage({
          command:{
            type: "initiate",
            data: cellData,
            selections: selections,
          },
          params: callParams.params
        })
      });

    }

    else if(callParams.request.type === "expr vals"){
      if(document.getElementById("localProgressBar")){
        var execution = (callParams.params.index/callParams.params.geneNames.length) * 100;
        document.getElementById("localProgressBar").style.width = execution + "%"
        document.getElementById("localProgressLabel").innerHTML = Math.floor(execution*10)/10 + "%";

        dataCtrl.getExpressionValuesSparseByCellIndex(callParams.request.data, 0, callParams.params.numCells, function(data){
          w.postMessage({
            command:{
              type: "process",
              data: data
            },
            params: callParams.params
          })
        })
      }
    }
    else if(callParams.request.type === "clean death"){

      thisController.localWorker.terminate();
      thisController.localWorker = undefined;
      document.getElementById("localProgressLabel").innerHTML = "Finishing"
      setTimeout(function(){callback(callParams.params.results);Ext.getCmp("localProgressBarWindow").close();},1);


    }
    else if(callParams.request.type === "abrupt death"){

      if(Ext.getCmp("localProgressBarWindow")){
        Ext.getCmp("localProgressBarWindow").close()
      }
      w.terminate();
      thisController.localWorker = undefined;
    }

  }
  return {
    abort: function(){
      w.postMessage({command:{type:"stop"}})
    }
  };
}

calculationController.prototype.calculateDEfor1selectionbyLocal = function(selectionA,callback, method){
  var cellSelCntr = new cellSelectionController();
  return this.calculateDELocal([cellSelCntr.getSelection(selectionA)], callback, method);
}
calculationController.prototype.calculateDEfor2selectionsbyLocal = function(selectionA, selectionB, callback, method){
  var cellSelCntr = new cellSelectionController();
  return this.calculateDELocal([cellSelCntr.getSelection(selectionA), cellSelCntr.getSelection(selectionB)], callback, method);
}

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


//////////////////////////////////////////

// TODO: Split this object to a new file

/**
 * Stores differential expression results and accompanying metadata
 * Singleton
 * @constructor
 */
function differentialExpressionStore() {
    if ( typeof differentialExpressionStore.instance === 'object' ){
	    return differentialExpressionStore.instance;
    };

    this.deSets = new Object();

    differentialExpressionStore.instance = this;
    return this;
};

/**
 * Get all available differential expression sets
 */
differentialExpressionStore.prototype.getAvailableDEsets = function() {
  var result = new Array();
  var availKeys = Object.keys(this.deSets);

  // Push key/display name pairs on an array
  for (var i = 0; i < availKeys.length; i++) {
    var curKey = availKeys[i];
    var name = curKey;
    var displayName = this.deSets[curKey].getName();

    var date = this.deSets[curKey].getStartTime().valueOf();
    result.push({'name': name, 'date': date, 'displayName': displayName});
  }
  return result;
};

/**
 * Get a de result by internal name
 */
differentialExpressionStore.prototype.getResultSetByInternalName = function(internalName) {
  //TODO: Implement some checks here
  return this.deSets[internalName];
}


/**
 * Add differential expression result to the store
 */
differentialExpressionStore.prototype.addResultSet = function(deset) {
  var newid = this.getUniqueId();
  this.deSets[newid] = deset;
  return newid;
};


/**
 * Generate a unique identifier for a de set
 * @private
 */
differentialExpressionStore.prototype.getUniqueId = function() {
  var dobj = new Date();
  var r = Math.floor(Math.random() * 1e12);
  var d = dobj.getTime();
  return  'deset_' +  d + '_' + r
}


///////////////////////////////
// TODO: Split this object into a new file

/**
 * Represents a differential expression result set and accompanying metadata
 * @constructor
 */
function deResultSet() {
  this.selectionA = null;
  this.selectionB = null;
  this.results = null;
  this.name = null;
  this.startTime = null;
  this.endTime = null;
};


/**
 * get the name
 */
deResultSet.prototype.getName = function() {
  return this.name;
};

/**
 * set the name
 */
deResultSet.prototype.setName = function(val) {
  this.name = val;
};

/**
 * get the startTime
 */
deResultSet.prototype.getStartTime = function() {
  return this.startTime;
};

/**
 * set the startTime
 */
deResultSet.prototype.setStartTime = function(val) {
  this.startTime = val;
};

/**
 * get the endTime
 */
deResultSet.prototype.getEndTime = function() {
  return this.endTime;
};

/**
 * set the endTime
 */
deResultSet.prototype.setEndTime = function(val) {
  this.endTime = val;
};


/**
 * Get the first cell selection (an array of cell ids)
 */
deResultSet.prototype.getSelectionA = function() {
  return this.selectionA;
};

/**
 * Set the first cell selection (an array of cell ids)
 */
deResultSet.prototype.setSelectionA = function(val) {
  this.selectionA = val;
};

/**
 * Set the second cell selection (an array of cell ids)
 */
deResultSet.prototype.getSelectionB = function() {
  return this.selectionB;
};


/**
 * Get the second cell selection (an array of cell ids)
 */
deResultSet.prototype.setSelectionB = function(val) {
  this.selectionB = val;
};


/**
 * Get the table of results
 */
deResultSet.prototype.getResults = function() {
  return this.results;
};


/**
 * Set the table of results
 */
deResultSet.prototype.setResults = function(val) {
  this.results = val;
};

