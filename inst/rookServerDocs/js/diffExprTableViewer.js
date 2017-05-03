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

  console.log('Initializing aspects table viewer...');

  this.declareExtJStypes();
  this.generateTables();


  diffExprTableViewer.instance = this;
};


/**
 * Generate tables for this view
 */
diffExprTableViewer.prototype.generateTables = function() {

  var areaHolder = Ext.getCmp('diffExprExtJS');

  // Handler function for table of de result sets
  var resultSetChangeSelectionListener = function(obj, selected, eOpts) {

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

    debugger;
    var table = Ext.getCmp('deResultsGenes');
    table.bindStore(pagingStore);

  }; // resultSetChangeSelectionListener


  // Table for the result sets
  var resultSetSelectionGrid = Ext.create('Ext.grid.Panel',{
    title: 'Differential Expression Result sets',
    id: 'deResultSetsTableExtJS',
    columns: [
      {text: 'Name', dataIndex: 'displayName', width: '80%' },
      {text: 'Internal name', dataIndex: 'name', width: '20%' }
      ],
    emptyText: 'No differential expression results are available',
    singleSelect: true,
    listeners: {
      'selectionchange': resultSetChangeSelectionListener
    }
  });

  // Table for the contents of the result sets
  var resultSetGenesGrid = Ext.create('Ext.grid.Panel', {
    title: 'Differentially expressed genes',
    id: 'deResultsGenes',
    empty: 'No differentially expressed genes to show.',
    columns: [
        {text: 'Name', dataIndex: 'name'},
        {text: 'M', dataIndex: 'M'},
        {text: 'Z', dataIndex: 'Z'},
        {text: 'fe', dataIndex: 'fe'},
        {text: 'highest', dataIndex: 'highest'}
      ]
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
        {name: 'fe', type: 'number'},
        {name: 'highest', type: 'boolean' }
      ]
    });
};

/**
 * Update the table from the differnetialExpresssionStore
 */
diffExprTableViewer.prototype.update = function() {

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
}
