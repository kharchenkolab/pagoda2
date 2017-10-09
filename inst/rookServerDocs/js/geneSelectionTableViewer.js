"use strict";
/**
 * Responsible for handling the gene
 * table creation and any required updates. Singlenton
 * @constructor
 */
function geneSelectionTableViewer()  {
    if (typeof geneSelectionTableViewer.instance === 'object') {
	return geneSelectionTableViewer.instance;
    }

    geneSelectionTableViewer.instance = this;

    this.selectionTable = null;
    this.geneFilter = [];
    this.initialize();

    this.autoShow = true; // changing a selection automatically reloads heatmap
};


/**
 * Generate the gene table
 * Here were are making the gene table and connecting it
 * to the appropriate data source provided by the dataController
 */
geneSelectionTableViewer.prototype.initialize = function() {
    var dataCntr = new dataController();
    var thisViewer = this;
    dataCntr.getGeneInformationStore(function(geneTableEntryStore) {

	// For checkboxes on table
	var geneSelectionTableSelectionModel =  Ext.create('Ext.selection.CheckboxModel', {});

	// Construct the table
	var geneTable = Ext.getCmp('geneSelectionTableViewerExtJS');
	thisViewer.selectionTable = Ext.create('Ext.grid.Panel',{
	    title: '',
	    id: 'extjsgeneselectiontable',
	    selModel: geneSelectionTableSelectionModel,
	    emptyText: 'No genes found',
	    store: geneTableEntryStore,
	    columns: [
		{ text: 'Name', dataIndex: 'genename', width: '80%',
		  // Custom render for links of gene names
		  renderer: function(value) {
		      return Ext.String.format(p2globalParams.misc.jaxGeneQueryFormatString,value,value)
		  },
		},
		{ text: 'Dispersion', dataIndex: 'dispersion', width: '20%' },
//		{ text: 'Score', dataIndex: 'score', width: '20%' }
	    ], // columns
	    height: '100%',
	    width: '100%',
	    singleSelect: false,
	    listeners: {
		// Hide the refresh button
		afterrender: function() {this.down('#refresh').hide();},
	    }, // listeners

	    tbar: Ext.create('Ext.PagingToolbar', {
		//store: geneTableEntryStore,
		displayInfo: true,
		prependButtons: true,

		items: [
		    {
			emptyText: 'Search...',
			xtype: 'textfield',
			id: 'selectionTableSearchBar',
			width: 100,
			listeners: {
			    'change': {buffer: 50, fn: function(f, newValue, oldValues, eOpts) {

			      var gstv = new geneSelectionTableViewer();
			      gstv.autoShow = false;
				var g = Ext.getCmp('extjsgeneselectiontable');
				var store = g.getStore();
				store.clearFilter();
				if (newValue !== '') {
				    store.filterBy(function(rec) {
					if (rec.get('genename').match(new RegExp(newValue,'i'))) {
					    return true;
					} else {
					    return false;
					} // if genename
				    }); // store filter by
				} // if new values
				(new geneSelectionTableViewer()).filterThroughSelection();
				gstv.autoShow = true;

			    }} //change listener and buffer
			} // listeners
		    }

		]
	    }), //tbar
	    listeners: {
		'selectionchange': function(selected, eOpts) {
		    var selectedGeneNames =  [];

		    var selectedItems = selected.getSelected();
		    selectedItems.each(function(item,index,length){
			selectedGeneNames.push(item.data.genename);
		    });

		    var geneSelCntr =  new geneSelectionController();
		    geneSelCntr.setSelection( selectedGeneNames,'geneTableSelection','geneTableSelection');

			      var gstv = new geneSelectionTableViewer();
			      if(gstv.autoShow ){

		    			    var heatmapV = new heatmapViewer();
			    heatmapV.setNamedSelectionToDisplayGenes('auto_geneTableSelection');
			    heatmapV.drawHeatmap();
			      }
		}

	    }, // listeners

	});
	geneTable.add(thisViewer.selectionTable);
  thisViewer.filterThroughSelection();
    });

}

geneSelectionTableViewer.prototype.generateTableFromSelection = function(geneSelections){
  var g = Ext.getCmp('extjsgeneselectiontable');
	var store = g.getStore();
	store.clearFilter();
	var thisViewer = new geneSelectionTableViewer();
  var geneSelCntrl = new geneSelectionController();
  var genes = {};
  geneSelections.forEach(function(selection){
    geneSelCntrl.getSelection(selection).genes.forEach(function(gene){
      genes[gene] = true;
    })
  });

  Ext.getCmp("selectionTableSearchBar").setValue("");
  thisViewer.geneFilter = Object.keys(genes);
  thisViewer.filterThroughSelection();
  thisViewer.raiseTab();
}
geneSelectionTableViewer.prototype.filterThroughSelection = function(){
	var geneSelTblView = (new geneSelectionTableViewer());
	var store = geneSelTblView.selectionTable.getStore();
  store.filterBy(function(rec) {
		if (geneSelTblView.geneFilter.includes(rec.get('genename'))) {
			return true;
		} else {
			return false;
		}
	})
}
geneSelectionTableViewer.prototype.raiseTab = function(){
  var tablesTab = Ext.getCmp('tablesTabExtJS');
  // FIXME: The tab order is hard-wired here
  tablesTab.setActiveTab("geneSelectionTableViewerExtJS");
}
