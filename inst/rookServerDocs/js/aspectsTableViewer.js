/*
 * Filename: aspectsTableViewer.js
 * Author:  Nikolas Barkas
 */

/**
 * Manages the aspects table
 * @constructor
 */
function aspectsTableViewer() {
  if (typeof aspectsTableViewer.instance == 'object'){
    return aspectsTableViewer.instance;
  }

  console.log('Initializing aspects table viewer');

  this.generateTable();

  aspectsTableViewer.instance = this;
};

aspectsTableViewer.prototype.generateTable = function() {
  var dataCntr = new dataController() ;

  var areaHolder = Ext.getCmp('aspectsExtJS');

  var aspectSelectionGrid = Ext.create('Ext.grid.Panel',{
    title: 'Available Aspects',
    id: 'aspectstable',
    empty: 'No aspects are defined',
    // TODO:...
  });

  var setsTableForAspects = Ext.create('Ext.grid.Panel',{
    title: 'Gene sets in this aspect',
    id: 'genesetsAspectTable',
    empty: 'No sets available'
  });

  var genesTableForAspects = Ext.create('Ext.grid.Panel',{
    title: 'Genes in the selected table',
    id: 'genesAspectsTable',
    empty: 'No genes selected'
  });

  areaHolder.add([{
    type: 'panel',
    title: '',
    header: false,
    region: 'north',
    split: true,
    layout: 'fit',
    width: '100%',
    height: '25%',
    items: [aspectSelectionGrid]
  },{
    type: 'panel',
    title: '',
    layout: 'fit',
    header: false,
    region: 'center',
    split: true,
    width: '100%',
    height: '25%',
    items: [setsTableForAspects]
  },{
    type: 'panel',
    title: '',
    layout: 'fit',
    header: false,
    region: 'south',
    split: true,
    width: '100%',
    height: '50%',
    items: [genesTableForAspects]
  }]);

}


