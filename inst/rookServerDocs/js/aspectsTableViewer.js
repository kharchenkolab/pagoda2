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

  this.generateTables();


  // Load the data
  // NOTE: There is a race condition here between the generation of the tables
  // and the ajax call
  var dataCntr = new dataController();
  dataCntr.getAvailableAspectsStore(function(aspectTableStore){
        table = Ext.getCmp('aspectstable');
        table.bindStore(aspectTableStore);
  });

  aspectsTableViewer.instance = this;
};

aspectsTableViewer.prototype.generateTables = function() {
  var dataCntr = new dataController() ;

  var areaHolder = Ext.getCmp('aspectsExtJS');

  // Selection change listener for aspects
  var aspectSelectionChangeListener = function(obj, selected, eOpts) {
    var selectedAspect = selected[0].data.name;
    console.log('selection listener', selectedAspect);

    // TODO: Update downstream
  };

  var aspectSelectionGrid = Ext.create('Ext.grid.Panel',{
    title: 'Available Aspects',
    id: 'aspectstable',
    empty: 'No aspects are defined',
    columns: [{text:'Name', dataIndex:'name', width:'100%'}],
    listeners: {
      'selectionchange': aspectSelectionChangeListener
    }
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
};

/**
 * Responds to an external aspect selection
 * @description responds to an aspect selection, it brings up the aspects tag
 * and selects the designated aspect
 */
aspectsTableViewer.prototype.showSelectedAspect = function(aspectIdentifier) {
  this.raiseTab();

  var table = Ext.getCmp('aspectstable');
  var index = table.getStore().find('name', aspectIdentifier);
  console.log('aspectId', aspectIdentifier);
  console.log('index',index);
  table.getSelectionModel().select(index);
};

/**
 * Raised the tab to of this panel
 */
aspectsTableViewer.prototype.raiseTab = function() {
  var tablesTab = Ext.getCmp('tablesTabExtJS');
  // FIXME: The tab order is hard-wired here
  tablesTab.setActiveTab(2);
};
