"use strict";
/**
 * Responsible for handling the tab with the genes
 * sets. Singleton
 * @constructor
 */
function geneSetsTableViewer() {
    if (typeof geneSetsTableViewer.instance === 'object') {
	    return geneSetsTableViewer.instance;
    }

    geneSetsTableViewer.instance = this;

    this.generateTables();
    this.autoShow = true;
};

/**
 * Generate the visual elements for this
 * @description This function generates two tables in the "Gene Sets of Interest" tab
 * The first table is to display the available gene sets and the second to
 * display the genes in these gene sets
 */
geneSetsTableViewer.prototype.generateTables = function() {
    var dataCntr = new dataController();

    var areaHolder = Ext.getCmp('geneSetsOfInterestExtJS');

    dataCntr.getGeneSetInformationStore(function(geneSetTableStore) {

	// Listener for selection change of the table of the genesets
	var geneSetSelectionChangeListener = function (obj, selected, eOpts) {
	    // Get the name of the selected set
	    // TODO: It would be good to have a different internal and display name
	    var selectedSet =  selected[0].data.genesetname;

	    // Get a store for the genes in this set
	    var dataCntr =  new dataController();
	    dataCntr.getGeneSetStoreByName(selectedSet, function(store) {

  		// Update the store on the sub table
  		var genesetTable =  Ext.getCmp('genesetGenesTable');
  		store.sort({property: 'dispersion', direction: 'DESC'});
  		genesetTable.bindStore(store);



	    });
	}

	var genesetSelectionGrid = Ext.create('Ext.grid.Panel',{
	    title: 'Available Gene Sets',
	    id: 'genesettable',
	    empty: 'No gene sets are defined',
	    store: geneSetTableStore,
	    columns: [
		{text: 'Name', dataIndex: 'genesetname', width: '30%'},
		{text: 'Description', dataIndex: 'shortdescription', width: '70%' }
	    ],
	    height: '100%',
	    width: '100%',
	    singleSelect: true,
	    listeners: {
		'selectionchange': geneSetSelectionChangeListener
	    },
	    tbar: Ext.create('Ext.PagingToolbar', {
		displayInfo: false,
		prependButtons: true,
		items: [
		    {
			emptyText: 'Search...',
			xtype: 'textfield',
			width: 100,
			listeners: {
			    'change': {buffer: 50, fn: function(f, newValue, oldValues, eOpts) {
			      var gstv = new geneSetsTableViewer();
			      gstv.autoShow = false;

				var g = Ext.getCmp('genesettable');
				var store = g.getStore();
				store.clearFilter();
				if (newValue != '') {
				    store.filterBy(function(rec) {
					if (rec.get('genesetname').match(new RegExp(newValue,'i')) ||
					    rec.get('shortdescription').match(new RegExp(newValue, 'i'))
					   ) {
					    return true;
					} else {
					    return false;
					}
				    }) // filterBy
				}
				gstv.autoShow = true;
			    }} // change listener and buffer
			}
		    }
		]
	    })
	});

	var selModel =  Ext.create('Ext.selection.CheckboxModel', {});

	// This is the table that will have the genes
	// of the currently selected pathway
	var geneTableForSets = Ext.create('Ext.grid.Panel', {
	    title: 'Genes in Selected Gene Set',
	    id: 'genesetGenesTable',
	    selModel: selModel,
	    empty: 'No genes to show',
	    columns: [
		{ text: 'Name', dataIndex: 'genename', width: '80%',
		  // Custom render for links of gene names
		  renderer: function(value) {
		      return Ext.String.format(p2globalParams.misc.jaxGeneQueryFormatString ,value,value)
		  },
		},
		{ text: 'Dispersion', dataIndex: 'dispersion', width: '20%' },
//		{ text: 'Score', dataIndex: 'score', width: '20%' }
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
		    geneSelCntr.setSelection( selectedGeneNames,'geneTableSelection','geneTableSelection');


var gstv = new geneSetsTableViewer();
if (gstv.autoShow) {
		    			    var heatmapV = new heatmapViewer();
			    heatmapV.setNamedSelectionToDisplayGenes('auto_geneTableSelection');
			    heatmapV.drawHeatmap();
}
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

				var g = Ext.getCmp('genesetGenesTable');
				var store = g.getStore();
				store.clearFilter();
				if (newValue !== '') {
				    store.filterBy(function(rec) {
					if (rec.get('genename').match(new RegExp(newValue,'i'))) {
					    return true;
					} else {
					    return false;
					}
				    }); // filterBy
				}// new value
			    }}
			    //change listener and buffer


			} // listeners
		    } // search textbox
		] // items
	    }) // tbar

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
	    items: [ genesetSelectionGrid  ]
	},{
	    type: 'panel',
	    layout: 'fit',
	    title: '',
	    header: false,
	    region: 'center',
	    split: true,
	    width: '100%',
	    height: '75%',
	    items: [geneTableForSets]
	}]);
    });
};


