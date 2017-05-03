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

  this.generateTables();

  diffExprTableViewer.instance = this;
};

diffExprTableViewer.prototype.generateTables = function() {
console.log('Hello!!!')

  var areaHolder = Ext.getCmp('diffExprExtJS');

  var resultSetSelectionGrid = Ext.create('Ext.grid.Panel',{
    title: 'DE Result sets',
    id: 'deResultSets',
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

}
