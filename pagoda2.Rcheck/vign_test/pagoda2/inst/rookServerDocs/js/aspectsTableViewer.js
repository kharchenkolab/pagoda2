"use strict";

/*
 * Filename: aspectsTableViewer.js
 * Author:  Nikolas Barkas
 */

/**
 * Manages the aspects table
 * @constructor
 */
function aspectsTableViewer() {
  if (typeof aspectsTableViewer.instance === 'object'){
    return aspectsTableViewer.instance;
  }

  // Generate the extjs elemetns
  this.generateTables();

  // Load the data
  // NOTE: There is a race condition here between the generation of the tables
  // and the ajax call
  var dataCntr = new dataController();
  dataCntr.getAvailableAspectsStore(function(aspectTableStore){
        var table = Ext.getCmp('aspectstable');
        table.bindStore(aspectTableStore);
  });

  aspectsTableViewer.instance = this;
};


/**
 * Update the geneset in the the aspects table
 */
aspectsTableViewer.prototype.updateGeneSetsInAspectTable = function(selectedAspect) {
      var dataCntr = new dataController();
      dataCntr.getAvailableGenesetsInAspectStore(selectedAspect, function(store) {
          var table = Ext.getCmp('genesetsAspectTable');
          store.sort({property: 'cz', direction: 'DESC'});
          table.bindStore(store);
      });
};


/**
 * Generate the tables
 * @private
 */
aspectsTableViewer.prototype.generateTables = function() {
  var dataCntr = new dataController() ;

  var areaHolder = Ext.getCmp('aspectsExtJS');

  // Selection change listener for aspects
  var aspectSelectionChangeListener = function(obj, selected, eOpts) {
    var selectedAspect = selected[0].data.name;
    var aspTableViewer = new aspectsTableViewer();
    aspTableViewer.updateGeneSetsInAspectTable(selectedAspect);
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

  var genesetSelectionChangeListener = function(obj, selected, eOpts) {
    // FIXME: Here we assume that they are all go terms
    // and they are also provided in the genesets of interest

    var selectedGeneset = selected[0].data.name;
    var goTerm = selectedGeneset.replace(/#.*# /,'');

    var dataCntr = new dataController();
    dataCntr.getGeneSetStoreByName(goTerm, function(store) {
      var genesetTable = Ext.getCmp('genesAspectsTable');
		  store.sort({property: 'dispersion', direction: 'DESC'});
		  genesetTable.bindStore(store);

    })

  }

  var setsTableForAspects = Ext.create('Ext.grid.Panel',{
    title: 'Gene sets in Aspect',
    id: 'genesetsAspectTable',
    empty: 'No sets available',
    columns: [
      {text: 'Name', dataIndex:'name', width:'20%'},
      {text: 'Description', dataIndex: 'shortdescription', width: '50%'},
      {text: 'Corrected Score', dataIndex: 'cz', width: '20%'},
      {text: 'Count', dataIndex: 'n', width: '10%'},
    ],
    listeners: {
      'selectionchange': genesetSelectionChangeListener
    }
  });

  var selModel =  Ext.create('Ext.selection.CheckboxModel', {});

  var genesTableForAspects = Ext.create('Ext.grid.Panel',{
    title: 'Genes in the selected Gene Set',
    id: 'genesAspectsTable',
    empty: 'No genes selected',
    selModel: selModel,
    columns: [
      {
        text: 'Name', dataIndex: 'genename', width: '80%',
        renderer: function(value) {return Ext.String.format(p2globalParams.misc.jaxGeneQueryFormatString ,value,value)}
      },
      {
        text: 'Dispersion',
        dataIndex: 'dispersion',
        width: '20%'
      }], //columns
      height: '100%',
      width: '100%',
      singleSelect: false,
      listeners: {
        'selectionchange': function(selected, eOpts) {
          		var selectedGeneNames =  [];

      		    var selectedItems = selected.getSelected();
      		    selectedItems.each(function(item,index,length){
      			    selectedGeneNames.push(item.data.genename);
      		    });

      		    var geneSelCntr =  new geneSelectionController();
      		    geneSelCntr.setSelection( selectedGeneNames,'geneTableSelection','geneTableSelection');

      		          			    var heatmapV = new heatmapViewer();
      			    heatmapV.setNamedSelectionToDisplayGenes('auto_geneTableSelection');
      			    heatmapV.drawHeatmap();
        } // selection change
      }, // listeners
      tbar: Ext.create('Ext.PagingToolbar', {
      		displayInfo: true,
		      prependButtons: true,
      }) //tbar
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
  table.getSelectionModel().select(index);
};

/**
 * Raised the tab to of this panel
 */
aspectsTableViewer.prototype.raiseTab = function() {
  var tablesTab = Ext.getCmp('tablesTabExtJS');
  // FIXME: The tab order is hard-wired here
  tablesTab.setActiveTab("aspectsExtJS");
};
