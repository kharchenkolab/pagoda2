"use strict";
/**
 * Responsible for handling the gene
 * table creation and any required updates. Singlenton
 * @constructor
 */
function geneTableViewer()  {
    if (typeof geneTableViewer.instance === 'object') {
	    return geneTableViewer.instance;
    }

    this.generateGeneTable();
    this.autoShow = true; // changing a selection automatically reloads heatmap

    geneTableViewer.instance = this;
};


/**
 * Generate the gene table
 * Here were are making the gene table and connecting it
 * to the appropriate data source provided by the dataController
 */
geneTableViewer.prototype.generateGeneTable = function() {
    var dataCntr = new dataController();

    dataCntr.getGeneInformationStore(function(geneTableEntryStore) {

	// For checkboxes on table
	var geneTableSelectionModel =  Ext.create('Ext.selection.CheckboxModel', {});

	// Construct the table
	var geneTable = Ext.getCmp('geneTableViewerExtJS');
	geneTable.add(Ext.create('Ext.grid.Panel',{
	    title: '',
	    id: 'extjsgenetable',
	    selModel: geneTableSelectionModel,
	    emptyText: 'No matching genes',
	    store: geneTableEntryStore,
	    columns: [
		{ text: 'Name', dataIndex: 'genename', width: '80%',
		  // Custom render for links of gene names
		  renderer: function(value) {
		      return Ext.String.format(p2globalParams.misc.jaxGeneQueryFormatString,value,value)
		  },
		},
		{ text: 'Dispersion', dataIndex: 'dispersion', width: '20%' },
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
			width: 100,
			listeners: {
			    'change': {buffer: 50, fn: function(f, newValue, oldValues, eOpts) {
			        var gtv = new geneTableViewer();
			        gtv.autoShow = false;

      				var g = Ext.getCmp('extjsgenetable');
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

			        gtv.autoShow = true;

			    }} //change listener and buffer
			} // listeners
		    },

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

        var gtv = new geneTableViewer();
        if(gtv.autoShow){
          var heatmapV = new heatmapViewer();
          heatmapV.setNamedSelectionToDisplayGenes('auto_geneTableSelection');
          heatmapV.drawHeatmap();
        }
		}

	    } // listeners
	}));

    });

}
