"use strict";

/**
 * Responsible for handling the overdispersed
 * gene table creation and any required updates. Singtenton
 * @constructor
 */
function odGeneTableViewer() {
    if (typeof odGeneTableViewer === 'object') {
	return odGeneTableViewer.instance;
    }

    this.generateTable();

    odGeneTableViewer.instance = this;
}

/**
 * Generate the table
 */
odGeneTableViewer.prototype.generateTable = function() {
    var dataCntr = new dataController();

    var callback =  function(odGeneTableEntryStore) {
	var selModel =  Ext.create('Ext.selection.CheckboxModel', {});
	var odGeneTable = Ext.getCmp('odGeneTableViewerExtJS');

	odGeneTable.add(Ext.create('Ext.grid.Panel', {
	    title: '',
	    id: 'extjsodgenetable',
	    selModel: selModel,
	    emptyText: 'No overdispersed genes found',
	    store: odGeneTableEntryStore,
	    columns: [
		{ text: 'Name', dataIndex: 'genename', width: '60%',
		  // Custom render for links of gene names
		  renderer: function(value) {
		      return Ext.String.format('<a href="http://www.informatics.jax.org/searchtool/' +
					       'Search.do?query={0}" target="_blank">{1}</a>',value,value)
		  },
		},
		{ text: 'Dispersion', dataIndex: 'dispersion', width: '20%' },
		{ text: 'Score', dataIndex: 'score', width: '20%' }
	    ], // columns
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
		    geneSelCntr.setSelection( selectedGeneNames, 'Current Selected Genes', 'geneTableSelection');
		}
	    }, // listeners
	    tbar: Ext.create('Ext.PagingToolbar', {
		displayInfo: true,
		prependButtons: true,
		items: [
		    {
			emptyText: 'Search...',
			xtype: 'textfield',
			width: 100,
			listeners: {
			    'change': {buffer: 50, fn: function(f, newValue, oldValue, eOpts) {
				var g = Ext.getCmp('extjsodgenetable');
				var store = g.getStore();
				store.clearFilter();
				if (newValue !== '') {
				    store.filterBy(function(rec) {
					if (rec.get('genename').match(new RegExp(newValue)) ) {
					// if (rec.get('genename') === newValue) {
					    return true;
					} else {
					    return false;
					} // if genename
				    }); // store.filterBy
				} // if newValue
			    }} // change listere and buffer
			}// listeners
		    }, // Search item
		    {
			type: 'button',
			text: 'Show selected',
			tooltip: 'Show selected gens in the main heatmap',
			glyph: 0xf0ce,
			handler: function() {
			    var heatmapV = new heatmapViewer();
			    heatmapV.setNamedSelectionToDisplayGenes('auto_geneTableSelection');
			    heatmapV.drawHeatmap();
			} // handler
		    }, //button item
		    {
			xtype: 'tbseparator'
		    },
		]
	    }) //tbar

	}))
    }; // callback definition

    dataCntr.getOdGeneInformationStore(callback);
}


