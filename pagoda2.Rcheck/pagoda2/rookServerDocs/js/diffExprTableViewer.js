"use strict";

/*
 * Filename: diffExprTableViewer.js
 * Author: Nikolas Barkas
 */

/**
 * Manages the differential expression results viewer
 * @constructor
 */
function diffExprTableViewer() {
  if (typeof diffExprTableViewer.instance === 'object'){
    return diffExprTableViewer.instance;
  }

  this.declareExtJStypes();
  this.generateTables();

  this.changeListenerEnabled = true;

  this.autoShow = true;

  diffExprTableViewer.instance = this;
};

/**
 * Enable the change listener that updates the subtable
 */
diffExprTableViewer.prototype.enableChangeListener = function() { this.changeListenerEnabled = true; }


/**
 * Disable the change listener that update the subtable.
 * @description this is required during main table changes to avoid errors
 */
diffExprTableViewer.prototype.disableChangeListener = function() { this.changeListenerEnabled = false; }


/**
 * Generate tables for this view
 */
diffExprTableViewer.prototype.generateTables = function() {

  var areaHolder = Ext.getCmp('diffExprExtJS');

  // Handler function for table of de result sets
  var resultSetChangeSelectionListener = function(obj, selected, eOpts) {

    var detv = new diffExprTableViewer();
    if (detv.changeListenerEnabled) {

      // Get the name of the de set
      var intName = selected[0].data.name;

      // Get the results
      var diffExprStore = new differentialExpressionStore();
      var deResults = diffExprStore.getResultSetByInternalName(intName);


      // Create a store and put the data in there
      var pagingStore = Ext.create('LocalJsonStore', {
        autoLoad: true,
        model: 'deResultTableEntry',
        pageSize: 100,
        localData: deResults.results
      });

      var table = Ext.getCmp('deResultsGenes');
      pagingStore.sort({property: 'absZ', direction: 'DESC'});
      table.bindStore(pagingStore);
    }
  }; // resultSetChangeSelectionListener


  var resultSelectionTbar = Ext.create('Ext.Toolbar');
  resultSelectionTbar.add(
      		    {
			type: "button",
			text: 'Help',
			tooltip: 'Display Help',
			glyph: 0xf128,
			handler: function() {
  pagHelpers.regC(1);
    Ext.create('Ext.window.Window', {
        height: 300,
        width: 400,
        title: 'Help: Differential Expression',
        scrollable: true,
        bodyPadding: 10,
        html: '<h2>Differential Expression</h2>' +
        '<p>The tables in this panel display the results of differential expression analyses run in the Actions Tab. Every result set is identified by the name provided at the time of the analysis and by an internal unique identifier. Click on one of the result entries on the top table to view the results in the bottom panel.</p>',
        constrain: true,
        closable: true,
        resizable: false
    }).show();
  }});
  /*
  resultSelectionTbar.add(
    {
      type: "button",
			text: 'Retrieve Cells',
			tooltip: 'Retrieve Cell Selection',
			glyph: 0xf0e2,
			handler: function() {

			 var selectedItems = Ext.getCmp("deResultSetsTableExtJS").getSelectionModel().getSelected();
			 if(selectedItems.length === 1){
			  var diffExprStore = differentialExpressionStore();
			  var resultsDEset = diffExprStore.getResultSetByInternalName(selectedItems.getAt(0).getData().name)
			  var selectionTextFields = [{
			    id: "selectionAName",
          xtype: "textfield",
          fieldLabel: "New Selection A Name",
          value: resultsDEset.getName() + "_A"
        }];

			  if(resultsDEset.getSelectionB()){
			    selectionTextFields.push({
			      id: "selectionBName",
            xtype: "textfield",
            fieldLabel: "New Selection B Name",
            value: resultsDEset.getName() + "_B"
			    });
			  }
			  selectionTextFields.push({
			    xtype:"button",
			    text: "Ok",
			    width: "20%",
			    height: "25%",
			    margin: "5 5 5 5",
			    handler: function(){

			      var selAName = Ext.getCmp("selectionAName").getValue();
			      var cellSelCntr = new cellSelectionController();
			      var re = new RegExp('[^A-Za-z0-9_]');
    			  if (selAName.length === 0) {
    				    Ext.MessageBox.alert('Error', 'You must enter a selection name');
    				    return;
    			  }
    			  else if (selAName.match(re) ) {
    				    Ext.MessageBox.alert('Error', 'The name must only contain letters, numbers and underscores (_)');
    				    return;
    			  }
    			  else if (cellSelCntr.getSelection(selAName)) {
    				    Ext.MessageBox.alert('Error', 'A selection with this name already exists!');
    				    return;
    				}
			      if(resultsDEset.getSelectionB()){
			        var selBName = Ext.getCmp("selectionBName").getValue();
			        if (selBName.length === 0) {
    				    Ext.MessageBox.alert('Error', 'You must enter a selection name');
    				    return;
    			    }
    			    else if (selBName.match(re) ) {
    				    Ext.MessageBox.alert('Error', 'The name must only contain letters, numbers and underscores (_)');
    				    return;
    			    }
    			    else if (cellSelCntr.getSelection(selBName)) {
    				    Ext.MessageBox.alert('Error', 'A selection with this name already exists!');
    				    return;
    				  }
    				  else if(selBName === selAName){
    				    Ext.MessageBox.alert("Error", "Selection A's name and Selection B's name must not be the same");
    				    return;
    				  }
    				  cellSelCntr.setSelection(resultsDEset.getSelectionB(), selBName);
			      }
			      cellSelCntr.setSelection(resultsDEset.getSelectionA(),selAName);
			      Ext.getCmp("retrieveCellSelectionsWindow").close();
			    }
			  });
			  selectionTextFields.push({
			    xtype:"button",
			    text: "Cancel",
			    width: "20%",
			    height: "25%",
			    margin: "5 5 5 5",
			    handler: function(){
			      Ext.getCmp("retrieveCellSelectionsWindow").close();
			    }
			  });

    Ext.create('Ext.window.Window', {
        height: 170,
        width: 400,
        title: 'Help: Differential Expression',
        id: "retrieveCellSelectionsWindow",
        scrollable: true,
        bodyPadding: 10,
        items: selectionTextFields,
        constrain: true,
        closable: true,
        resizable: false
    }).show();
			} else {
			  Ext.messagebox.alert("Please select only one differential expression result at a time.");
			}
			}
    });
*/
  // Table for the result sets
  var resultSetSelectionGrid = Ext.create('Ext.grid.Panel',{
    title: 'Differential Expression Result sets',
    id: 'deResultSetsTableExtJS',
    columns: [
      {text: 'Name', dataIndex: 'displayName', flex: 4.5 },
      {text: 'Date', dataIndex: 'date', flex: 3.5, renderer: function(value){
        return (new Date(value)).toUTCString();
      }},
      {text: 'Internal name', dataIndex: 'name', flex: 2, hidden:true}
      ],
    emptyText: 'No differential expression results are available',
    singleSelect: true,
    listeners: {
      'selectionchange': resultSetChangeSelectionListener
    },
    tbar: resultSelectionTbar
  });

var geneTableSelectionModel =  Ext.create('Ext.selection.CheckboxModel', {});
  // Table for the contents of the result sets
  var resultSetGenesGrid = Ext.create('Ext.grid.Panel', {
    title: 'Differentially expressed genes',
    id: 'deResultsGenes',
    selModel: geneTableSelectionModel,
    singleSelect: false,
    empty: 'No differentially expressed genes to show.',
    columns: [
        {text: 'Name', dataIndex: 'name', renderer: function(value) {
		      return Ext.String.format(p2globalParams.misc.jaxGeneQueryFormatString ,value,value)
		  },},
        {text: 'Log2(fc)', dataIndex: 'M'},
        {text: 'Z-score', dataIndex: 'Z'},
        {text: 'absolute Z-score', dataIndex: 'absZ'},
        {text: 'Fraction Expressing', dataIndex: 'fe'},
        {text: 'highest', dataIndex: 'highest'}
      ],

      ///
      tbar: Ext.create('Ext.PagingToolbar', {
		//store: geneTableEntryStore,
        listeners: {
  		    afterrender: function() {
            this.child('#refresh').hide()
  		    }
        },

		displayInfo: false,
		prependButtons: true,

		items: [
{
			emptyText: 'Search...',
			xtype: 'textfield',
			width: 100,
			listeners: {
			    'change': {buffer: 50, fn: function(f, newValue, oldValues, eOpts) {

			      var detb = new diffExprTableViewer();
            detb.autoShow = false;

    				var g = Ext.getCmp('deResultsGenes');
    				var store = g.getStore();
    				store.clearFilter();
    				if (newValue !== '') {
    				    store.filterBy(function(rec) {
    					if (rec.get('name').match(new RegExp(newValue,'i'))) {
    					    return true;
    					} else {
    					    return false;
    					} // if genename
    				    }); // store filter by
    				} // if new values

    							    detb.autoShow = true;
    			    }} //change listener and buffer
    			} // listeners


		    },

/*
		    {
    			type: "button",
    			text: 'Show selected',
    			tooltip: 'Show selected genes in main heatmap',
    			glyph: 0xf0ce,
    			handler: function() {
    			    var heatmapV = new heatmapViewer();
    			    heatmapV.setNamedSelectionToDisplayGenes('auto_geneTableSelection');
    			    heatmapV.drawHeatmap();
    			} //handler
		    }, */ //button
		    {
		      type: 'button',
		      text: 'Export',
		      tooltip: 'Download results as CSV file',
		      glyph:  0xf0ed,
		      handler: function() {
		        
		        
            // Generate CSV
            var grid = Ext.getCmp('deResultsGenes');
            var csvFile = pagHelpers.DEproxydataToCSV(grid.store.proxy.data);

            // Download CSV
            var blob = new Blob([csvFile], { type: 'text/csv;charset=utf-8;' });
            pagHelpers.downloadURL(blob, 'diffExpr.csv');
		      }
		    },
		    {xtype: 'tbseparator'},

	  	]
	    }), //tbar



      ///
      listeners: {
		'selectionchange': function(selected, eOpts) {
		    var selectedGeneNames =  [];

		    var selectedItems = selected.getSelected();
		      selectedItems.each(function(item,index,length){
			    selectedGeneNames.push(item.data.name);
		    });

		    var geneSelCntr =  new geneSelectionController();
		    geneSelCntr.setSelection( selectedGeneNames,'geneTableSelection','geneTableSelection');


			      var detb = new diffExprTableViewer();
            if(detb.autoShow) {
		        			    var heatmapV = new heatmapViewer();
    			    heatmapV.setNamedSelectionToDisplayGenes('auto_geneTableSelection');
    			    heatmapV.drawHeatmap();
            }

		}}
  });

  areaHolder.add({
    type: 'panel',
    title: '',
    header: false,
    region: 'north',
    split: true,
    layout: 'fit',
    width: '100%',
    height: '25%',
    items: [resultSetSelectionGrid]
  },
  {
    type: 'panel',
    title: '',
    header: false,
    region: 'center',
    split: true,
    layout: 'fit',
    width: '100%',
    height: '75%',
    items: [resultSetGenesGrid]
  });
};

/**
 * Declares ExtJS datatypes relevant to this object
 * @private
 */
diffExprTableViewer.prototype.declareExtJStypes = function() {
   // For table that allows to view specific de set
    Ext.define('deResultSetsEntry', {
    	extend: 'Ext.data.Model',
    	fields: [
    	    {name: 'name', type: 'string'},
    	    {name: 'date', type: 'number'},
    	    {name: 'displayName', type: 'string'}
    	]
    });

    // For table the allows browsing results
    Ext.define('deResultTableEntry', {
      extend: 'Ext.data.Model',
      fields: [
        {name: 'name', type: 'string'},
        {name: 'M', type: 'number'},
        {name: 'Z', type: 'number'},
        {name: 'absZ', type: 'number'},
        {name: 'fe', type: 'number'},
        {name: 'highest', type: 'boolean' }
      ]
    });
};

/**
 * Update the table from the differnetialExpresssionStore
 */
diffExprTableViewer.prototype.update = function() {
  this.disableChangeListener();


  // Get the available DEsets
  var diffExprStore = new differentialExpressionStore();
  var desets = diffExprStore.getAvailableDEsets();

  // Create a store and put the data in there
  var pagingStore = Ext.create('LocalJsonStore', {
    autoLoad: true,
    model: 'deResultSetsEntry',
    pageSize: 100,
    localData: desets
  });

  // Update the store
  var diffExprTable = Ext.getCmp('deResultSetsTableExtJS');
  diffExprTable.bindStore(pagingStore);

    this.enableChangeListener();

}

/**
 * Raise the tab with the tables and hightlight a de set by it's internal name
 * @param internalName the internal name of the dataset
 */
diffExprTableViewer.prototype.showSelectedSet = function(internalName) {
  this.raiseTab();

  var table = Ext.getCmp('deResultSetsTableExtJS');
  var index = table.getStore().find('name', internalName);
  table.getSelectionModel().select(index);

}

/**
 * Raise the tab that holds the tables for this object
 */
diffExprTableViewer.prototype.raiseTab = function() {
  var tablesTab = Ext.getCmp('tablesTabExtJS');
  // FIXME: The tab order is hard-wired here
  tablesTab.setActiveTab("diffExprExtJS");
}
