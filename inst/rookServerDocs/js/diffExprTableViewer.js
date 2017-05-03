"use strict";

/*
 * Filename: diffExprTableViewer.js
 * Author: Nikolas Barkas
 */

/**
 * Manages the differential expression results viewer
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

diffExprTableViewer.prototype.generateTables = function() {

  var areaHolder = Ext.getCmp('diffExprExtJS');

  var resultSetSelectionGrid = Ext.create('Ext.grid.Panel',{
    title: 'DE Result sets',
    id: 'deResultSetsTableExtJ',
    columns: [
      {text: 'Name', dataIndex: 'displayName', width: '80%' },
      {text: 'Internal name', dataIndex: 'name', width: '20%' }
      ],
    empty: 'No differential expression results are available. You can perform differential expression analysis using the Actions panel.',
  });

  var resultSetGenesGrid = Ext.create('Ext.grid.Panel', {
    title: 'Differentially expressed genes',
    id: 'deResultsGenes',
    empty: 'No differentially expressed genes to show.'
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

diffExprTableViewer.prototype.declareExtJStypes = function() {
    Ext.define('deResultSetsEntry', {
    	extend: 'Ext.data.Model',
    	fields: [
    	    {name: 'name', type: 'string'},
    	    {name: 'displayName', type: 'string'},
    	]
    });
};

/**
 * Update the table from the differnetialExpresssionStore
 */
diffExprTableViewer.prototype.update = function() {
  var diffExprStore = new differentialExpressionStore();
    //TODO: Update freom the store
  var desets = diffExprStore.getAvailableDEsets();

  // Create a store and put the data in there
  var pagingStore = Ext.create('LocalJsonStore', {
    autoLoad: true,
    model: 'deResultSetsEntry',
    pageSize: 100,
    localData: desets
  });

  // Update the store
  var diffExprTable = Ext.getCmp('deResultSetsTableExtJ');
  diffExprTable.bindStore(pagingStore);
}
