"use strict";

/*
 * Filename: actionPanelUIcontroller.js
 * Author: Nikolas Barkas
 * Date: May 2017
 */


/**
 * Manages the action panel interface. Singleton
 * @constructor
 */
function actionPanelUIcontroller() {
    if (typeof actionPanelUIcontroller.instance === 'object') {
	return actionPanelUIcontroller.instance;
    }

    this.generateCellSelectionStore();
    this.generateUI();

    // Setup listener for selection change
    var evtBus = new eventBus();
    evtBus.register("cell-selection-updated", null, function() {
	    var actPaneUICntr = new actionPanelUIcontroller();
	    actPaneUICntr.syncCellSelectionStore();
    });


    this.currentDErequest = null;
    actionPanelUIcontroller.instance = this;

};


/**
 * Generate the UI controls
 * @private
 */
actionPanelUIcontroller.prototype.generateUI = function() {
    var actionsTab = Ext.getCmp('actions-ui-tab');
    var UIcontroller = this;

    var actionsInnerTab = Ext.create('Ext.TabPanel', {
	layout: 'fit',
	width: '100%',
	height: '100%',
	tabBarPosition: 'top',
	items: [
	    {
		layout: 'fit',
		title: 'Differential Expression',
		id: 'differentialExpressionTab',
		glyph: 0xf14e, //fa-compass
		tooltip: 'Perform differential expression between two groups of cells',
		height: '100%',
		width: '100%',
	    },
	    {
		layout: 'fit',
		title: 'Gene to Gene Expression',
		id: 'expressionScatterPlotTab',
		glyph: 0xf192, //fa-dot-in-circle
		tooltip: 'Perform differential expression between two genes in one group of cells',
		height: '100%',
		width: '100%',
	    },
	    {
		layout: 'fit',
		title: 'Meta Data Comparison',
		id: 'metaDataBarGraphTab',
		glyph: 0xf080, //fa-bar-chart
		tooltip: 'Examine membership of cells between different clusters of meta data',
		height: '100%',
		width: '100%',
	    }
	    /*,
	    {
		layout: 'fit',
		title: 'Enrichment',
		glyph: 0xf200, // pie chart
		tooltip: 'Perform enrichment analysis',
		height: '100%',
		width: '100%'
	    } */
	]
    });

    // Data store for the available diff expr methods
    var deMethodsStore = Ext.create('Ext.data.Store', {
	id: 'availableDEmethodStore',
	fields: [{name: 'name', type: 'string'},
	       {name:'displayname', type: 'string'}]
    });

    // Populate the store
    var calcCntr = new calculationController();
    var availableMethods = calcCntr.getAvailableDEmethods();


    for (var i in availableMethods) {
	    deMethodsStore.add({name: availableMethods[i].name, displayname: availableMethods[i].displayName});
    }
    this.generateMetaDataStore();
    
    var deTab = Ext.getCmp('differentialExpressionTab');
    var espTab = Ext.getCmp('expressionScatterPlotTab');
    var mdbgTab = Ext.getCmp('metaDataBarGraphTab');
    var formPanelDE =  Ext.create('Ext.form.Panel', {
    	id: 'formPanelDE',
    	height: '100%',
    	width: '100%',
    	bodyPadding: 10,
    	items: [
        {
          xtype: 'radiogroup',
          name: 'analysisType',
          fieldLabel: 'Analysis Type',
          items: [
            {
              boxLabel: 'Selection vs Selection',
              name: 'analysisTypeSelection',
              inputValue: 'vsSelection',
              checked: true
            },
            {
              boxLabel: 'Selection vs Background',
              name: 'analysisTypeSelection',
              inputValue: 'vsBackground'
            }
          ],

          listeners: {
            change: function(obj, newValue, oldValue, eOpts) {
              var selectionBcontrol = Ext.getCmp('selectionB');
              //debugger;
              if (newValue.analysisTypeSelection == 'vsSelection') {
                selectionBcontrol.enable();
              } else if (newValue.analysisTypeSelection == 'vsBackground') {
                selectionBcontrol.disable();
              } else {
                //Something is wrong
              }
            }
          }
        },
    	  {
    	    id: 'selectionA',
    	    xtype: 'combo',
    	    fieldLabel: 'Main Cell Selection',
    	    queryMode: 'local',
    	    editable: false,
    	    store: Ext.data.StoreManager.lookup('cellSelectionStoreForDE'),
    	    displayField: 'displayname',
    	    valueField: 'selectionname'
    	},
    	{
    	    id: 'selectionB',
    	    xtype: 'combo',
    	    fieldLabel: 'Reference Cell Selection',
    	    queryMode: 'local',
    	    editable: false,
    	    store: Ext.data.StoreManager.lookup('cellSelectionStoreForDE'),
    	    displayField: 'displayname',
    	    valueField: 'selectionname'
    	},{
    	    id: 'selectionMethod',
    	    xtype: 'combo',
    	    fieldLabel: 'Method', // TODO add help pane
    	    queryMode: 'local',
    	    editable: false,
    	    store: Ext.data.StoreManager.lookup('availableDEmethodStore'),
    	    displayField: 'displayname',
    	    valueField: 'name'
    	},{
    	  id: 'resultName',
    	  xtype: 'textfield',
    	  fieldLabel: 'Name for results'
    	},{
    	    xtype: 'button',
    	    text: 'Run differential expression',
    	    name: 'runDEbutton',
    	    id: 'runDEbutton',
    	    handler: UIcontroller.runAnalysisClickHandler
    	},
    	{
    	  xtype: 'button',
    	  text: 'Stop',
    	  name: 'stopDEbutton',
    	  id: 'stopDEbutton',
    	  handler: UIcontroller.stopAnalysisClickHandler,
    	  disabled: true
    	},
    	{
    	  xtype: 'button',
    	  glyph: 0xf128,
    	  text: 'Help',
    	  handler: UIcontroller.showDEhelpDialog
    	}



    	] //items
    });
    
    //Gene expression scatter chart
    var formPanelESP =  Ext.create('Ext.form.Panel', {
    	id: 'formPanelESP',
    	height: '100%',
    	width: '100%',
    	bodyPadding: 10,
    	items: [
    	  {
          xtype: 'radiogroup',
          name: 'analysisType',
          fieldLabel: 'Plotted Cells',
          items: [
            {
              boxLabel: 'From Selection',
              name: 'analysisTypeSelection',
              inputValue: 'vsSelection',
              disabled: true
            },
            {
              boxLabel: 'All Cells',
              name: 'analysisTypeSelection',
              inputValue: 'vsBackground',
              checked: true
            }
          ],

          listeners: {
            change: function(obj, newValue, oldValue, eOpts) {
              var selectionControl = Ext.getCmp('cellSelectionESP');
              //debugger;
              if (newValue.analysisTypeSelection == 'vsSelection') {
                selectionControl.enable();
              } else if (newValue.analysisTypeSelection == 'vsBackground') {
                selectionControl.disable();
              } else {
                //Something is wrong
              }
            }
          }
        },
    	  {
    	    id: 'cellSelectionESP',
    	    xtype: 'combo',
    	    fieldLabel: 'Reference Cell Selection',
    	    queryMode: 'local',
    	    editable: false,
    	    disabled: true,
    	    store: Ext.data.StoreManager.lookup('cellSelectionStoreForDE'),
    	    displayField: 'displayname',
    	    valueField: 'selectionname',
    	    //disabled: true
    	  },
    	  {
    	    xtype: 'textfield',
    	    id: 'geneA',
    	    fieldLabel: "Gene A",
    	  },
    	  {
    	    xtype: 'textfield',
    	    id: 'geneB',
    	    fieldLabel: "Gene B",
    	  },
    	  {
    	    xtype: 'button',
    	    text: 'Build Plot',
    	    margin: '5 5 5 5',
    	    handler: UIcontroller.generateESPwindow
    	  },
    	  {
    	    xtype: 'button',
    	    glyph: 0xf128,
    	    text: 'help',
    	    margin: '5 5 5 5',
    	    handler: UIcontroller.showESPhelpDialog
    	  }
    	]
    });
    //Meta Data Bar Graph
    var formPanelMDBG = Ext.create('Ext.form.Panel',{
      id: 'formPanelMDBG',
    	height: '100%',
    	width: '100%',
    	bodyPadding: 10,
      items: [
        {
          id: 'metaDataSelectionA',
          xtype: 'combo',
          fieldLabel: 'Reference Meta Data Clusters',
          queryMode: 'local',
          editable: false,
          store:Ext.data.StoreManager.lookup('metaDataSelectionStoreForDBG'),
          displayField: 'displayName',
          valueField: 'internalName'
        },
        {
          id: 'metaDataSelectionB',
          xtype: 'combo',
          fieldLabel: 'Examined Meta Data Clusters',
          queryMode: 'local',
          editable: false,
          store: Ext.data.StoreManager.lookup('metaDataSelectionStoreForDBG'),
          displayField: 'displayName',
          valueField: 'internalName'
        },
        {
    	    xtype: 'button',
    	    text: 'Build Plot',
    	    margin: '5 5 5 5',
    	    handler: UIcontroller.generateMDBGwindow
    	  },
    	  {
    	    xtype: 'button',
    	    glyph: 0xf128,
    	    text: 'help',
    	    margin: '5 5 5 5',
    	    handler: UIcontroller.showMDBGhelpDialog
    	  }
      ]
    })
    
    deTab.add(formPanelDE);
    espTab.add(formPanelESP);
    mdbgTab.add(formPanelMDBG)
    actionsTab.add(actionsInnerTab);
};


/**
 * Disable the run button (and enable the stop button) for de analysis
 */
actionPanelUIcontroller.prototype.disableRunButton = function() {
  var form = Ext.getCmp("formPanelDE").getForm();
  var button = Ext.getCmp('runDEbutton');
  button.setText('Please wait...');
  button.disable();

  var stopButton = Ext.getCmp('stopDEbutton');
  stopButton.enable();

}

/**
 * Enable the run button (and disable the stop button) for de analysis
 */
actionPanelUIcontroller.prototype.enableRunButton = function() {
  var form = Ext.getCmp("formPanelDE").getForm();
  var button = Ext.getCmp('runDEbutton');
  button.setText('Run differential expression...');
  button.enable();

  var stopButton = Ext.getCmp('stopDEbutton');
  stopButton.disable();
}



/**
 * Show help dialog for differential analysis
 */
actionPanelUIcontroller.prototype.showDEhelpDialog = function() {
    Ext.create('Ext.window.Window', {
      height: 300,
      width: 400,
      title: 'Help: Differential expression',
      scrollable: true,
      bodyPadding: 10,
      html: '<h2>Running differential expression</h2>' +
        '<p>You can use this form to run differential expression in two different modes: \'Selection vs Selection\' or \'Selection vs Background\'. In both cases your results will be displayed on the right hand column under the \'Differential Expression\' panel.</p>' +
        '\'Selection vs Selection\' will compare two gene selections specified. To run this type of differential expression you will need to have specified two different cells groups. You can do that in several ways. Cell selections can be defined in the embedding by drag selecting, in the dendrogram (see the dendrogram help for more details) and in the metadata by selecting all cells in a particular cluster. Your cell selections are available in the \'Cell selection\' panel which also allows you to highlight them to confirm their identity. In order to run differential expression select cell selections to compare using the drop down menus and then select the method you want to use. Methods can either be local (run in the browser) or remote (run in the supporting server). Finally enter a name for your selection and click on \'Run differential expression\'. After a short wait your results will appear in the right hand panel.</p>' +
        '<p>In the \'Selection vs Background\' mode you can perform differntial expression between one cell selection and everything else. This is identify genes that are highly expressed in your selection but not other cells. The procedure is similar to the one described above but does not require the selection of two cell sets.</p>',
      constrain: true,
      closable: true,
      resizable: false
    }).show();
    pagHelpers.regC(94);
}
/**
 * Show help dialog for Expression Scatter Plot
 */
actionPanelUIcontroller.prototype.showESPhelpDialog = function (){
  Ext.create('Ext.window.Window', {
      height: 300,
      width: 400,
      title: 'Help: Expression Scatter Plots',
      scrollable: true,
      bodyPadding: 10,
      html: '<h2>Plotting differential expression of two genes in a cell selection</h2>' +
        '<p></p>'
        ,
      constrain: true,
      closable: true,
      resizable: false
    }).show();
}
/**
 * Generates an ESP window if the data provided on the ESP tab is valid 
 */
actionPanelUIcontroller.prototype.generateESPwindow = function(){
  var form = Ext.getCmp("formPanelESP").getForm();
  
  var cellSelection = form.findField("cellSelectionESP").getValue();
  var geneA = form.findField("geneA").getValue();
  var geneB = form.findField("geneB").getValue();
  
  if(geneA.length === 0){
    Ext.MessageBox.alert('Warning',"Please provide a gene in the Gene A field.");
  }
  else if(geneB.length === 0){
    Ext.MessageBox.alert('Warning',"Please provide a gene in the Gene B field.");
  }
  else{
    var len;
    (new dataController()).getCellOrder(function(data){len = data.length})
    if(!len){return}
    (new dataController()).getExpressionValuesSparseByCellIndexUnpacked(Array(geneA,geneB),0,len,false, function(data){
      
      if(data.DimNames2.length < 2){
        Ext.MessageBox.alert("Error", "One or more of the gene names provided could not be found in the provided dataset.");
        return;
      }
      var geneMatrix = data.getFullMatrix();
      
      var scatterData = {
        data: geneMatrix.array,
        xLabel: geneMatrix.colnames[0],
        yLabel: geneMatrix.colnames[1],
        title: "Differential Gene Expression"
      };

      (new graphViewer(scatterData, "scatter")).render();
      
    })
  }
}

/**
 * Generates an MDBG window if the data provided on the MDBG tab is valid 
 */
actionPanelUIcontroller.prototype.generateMDBGwindow = function(){
  var form = Ext.getCmp("formPanelMDBG").getForm();
  
  var metaDataReference = form.findField("metaDataSelectionA").getValue();
  var metaDataComparison = form.findField("metaDataSelectionB").getValue();
  
  if(!(metaDataReference)){
    Ext.MessageBox.alert("Warning","Please specify your reference meta data clustering");
    return;
  }
  if(!(metaDataComparison)){
    Ext.MessageBox.alert("Warning","Please specify your comparison meta data clustering");
    return;
  }
  var dataCtrl = new dataController();
  var barGraphData = {} 
  dataCtrl.getCellMetadata(function(data){
    
    barGraphData.data = [];
    for(var i = 0; i < data[metaDataReference].palette.length; i++){
      var nextArray = [];
      for(var j = 0; j < data[metaDataComparison].palette.length; j++){
        nextArray.push(0);
      }
      barGraphData.data.push(nextArray);
    }
    for(var cellId in data[metaDataReference].data){barGraphData.data[data[metaDataReference].data[cellId]][data[metaDataComparison].data[cellId]]++}
    barGraphData.compPalette = data[metaDataComparison].palette;
    barGraphData.refPalette = data[metaDataReference].palette;
    barGraphData.title = "Cluster Membership Bar graph";
    barGraphData.xLabel = data[metaDataReference].displayname;
    barGraphData.yLabel = data[metaDataComparison].displayname + " cells % Membership";
    (new graphViewer(barGraphData, "bar")).render();
  });
  
  
}
/**
 * Show help dialog for Meda Data Bar Graph
 */
actionPanelUIcontroller.prototype.showMDBGhelpDialog = function(){
  Ext.create('Ext.window.Window', {
      height: 300,
      width: 400,
      title: 'Help: Expression Scatter Plots',
      scrollable: true,
      bodyPadding: 10,
      html: '<h2>Displaying differential inclusion of cells within the clusters of each selected metadata grouping</h2>' +
        '<p></p>'
        ,
      constrain: true,
      closable: true,
      resizable: false
    }).show();
}

/**
 * Click handler for stop button of DE analysis
 * @private
 */
actionPanelUIcontroller.prototype.stopAnalysisClickHandler = function() {
 var actionUI = new actionPanelUIcontroller();
 actionUI.currentDErequest.abort();
 actionUI.currentDErequest = null;
 actionUI.enableRunButton();
}


/**
 * Click handler for run button of DE analysis
 * @private
 */
actionPanelUIcontroller.prototype.runAnalysisClickHandler = function() {

  var form = Ext.getCmp("formPanelDE").getForm();

  var analysisType = form.findField("analysisType").getValue();
  var selectionA = form.findField("selectionA").getValue();
  var selectionB = form.findField("selectionB").getValue();
  var method = form.findField('selectionMethod').getValue();
  var resultName = form.findField("resultName").getValue();

  if (method === null) {
        Ext.MessageBox.alert('Warning', 'Please enter a method for the differential expression',function(){});
  } else if (resultName === '') {
        Ext.MessageBox.alert('Warning', 'Please enter a name for the results',function(){});
  } else {
    if (analysisType.analysisTypeSelection == 'vsSelection') {
      if (selectionA === selectionB) {
          Ext.MessageBox.alert('Warning', 'Please select a different set for A and B');
      } else {

          var actionUI = new actionPanelUIcontroller();
          actionUI.disableRunButton();

          var calcCntr = new calculationController();

          actionUI.currentDErequest = calcCntr.calculateDEfor2selections(selectionA, selectionB, 'remoteDefault',  function(results,start) {
            actionUI.enableRunButton();
              actionUI.currentDErequest = null;

              // Get the cell names in the selection for storing
              var cellSelCntr = new cellSelectionController();
              var selAcells = cellSelCntr.getSelection(selectionA);
              var selBcells = cellSelCntr.getSelection(selectionB);

              // Make a deResult set for saving the results
              // and save metadata related to this de result
              
              var end = new Date();
              var resultSet = new deResultSet();
              resultSet.setResults(results);
              resultSet.setName(resultName);
              resultSet.setSelectionA(selAcells);
              resultSet.setSelectionB(selBcells);
              resultSet.setStartTime(start);
              resultSet.setEndTime(end);
              // Save this de result set in the differentialExpresionStore
              var diffExprStore = new differentialExpressionStore();
              var setId = diffExprStore.addResultSet(resultSet);

              // Notify the DE results table to updata from the store
              var diffExpreTblView = new diffExprTableViewer();
              diffExpreTblView.update();

              diffExpreTblView.showSelectedSet(setId);

              // TODO: Change focus to the table and hightlight new de set
          } );
      }  // if .. else
    } else if (analysisType.analysisTypeSelection == 'vsBackground') {
          var actionUI = new actionPanelUIcontroller();
          actionUI.disableRunButton();
          var calcCntr = new calculationController();
          actionUI.currentDErequest = calcCntr.calculateDEfor1selection(selectionA, 'remoteDefault',  function(results,start) {
              actionUI.enableRunButton();
              actionUI.currentDErequest = null;
              // Get the cell names in the selection for storing
              var cellSelCntr = new cellSelectionController();
              var selAcells = cellSelCntr.getSelection(selectionA);

              // Make a deResult set for saving the results
              // and save metadata related to this de result
              var end = new Date();
              var resultSet = new deResultSet();
              resultSet.setResults(results);
              resultSet.setName(resultName);
              resultSet.setSelectionA(selAcells);
              resultSet.setStartTime(start);
              resultSet.setEndTime(end);
              // Save this de result set in the differentialExpresionStore
              var diffExprStore = new differentialExpressionStore();
              var setId = diffExprStore.addResultSet(resultSet);

              // Notify the DE results table to updata from the store
              var diffExpreTblView = new diffExprTableViewer();
              diffExpreTblView.update();

              diffExpreTblView.showSelectedSet(setId);
          } );
    } // else.. if
  }
} // runAnalysisClickHandler


/**
 * Generate the cell selection store for populating the dropdowns
 * @private
 */
actionPanelUIcontroller.prototype.generateCellSelectionStore = function() {
    Ext.create('Ext.data.Store', {
	storeId: 'cellSelectionStoreForDE',
	fields: [
		{ name: 'selectionname', type: 'string'},
		{ name: 'displayname', type: 'string'}
	],
	autoLoad: true
    });
}


/**
 * Update the cell selection store for populating the dropdowns
 * with information from the selection controller
 * @private
 */
actionPanelUIcontroller.prototype.syncCellSelectionStore = function() {
    // Get the store for the table and empty it
    var store = Ext.data.StoreManager.lookup('cellSelectionStoreForDE');
    store.removeAll();

    // Repopulate the store
    var cellSelCntr =  new cellSelectionController();
    var availSelections = cellSelCntr.getAvailableSelections();

    for (var sel in availSelections) {
    	var selName = availSelections[sel];
    	var selDisplayName =  cellSelCntr.getSelectionDisplayName(selName);

    	store.add({selectionname: selName, displayname: selDisplayName});
    }// for
}

actionPanelUIcontroller.prototype.generateMetaDataStore = function(){
  
  var mdStore = Ext.create('Ext.data.Store',{
    storeId: 'metaDataSelectionStoreForDBG',
    fields: [
      {name:'internalName', type:'string'},
      {name:'displayName', type:'string'}
    ],
    autoLoad: true
  })
  var dataCntr = new dataController();
  dataCntr.getCellMetadata(function(data) {
    for(var key in data){
      mdStore.add({internalName: key, displayName: data[key].displayname});
    }  
  });
}



