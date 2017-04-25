
/**
 * Manages the cell selection interface
 * @constructor
 */
function cellSelectionUIcontroller() {
    if (typeof cellSelectionUIcontroller.instance === 'object') {
	return cellSelectionUIcontroller.instance;
    }   

    cellSelectionUIcontroller.instance = this;

    this.generateCellSelectionStore();
    this.generateUI();

    // Setup listener for selection change
    var evtBus = new eventBus();
    evtBus.register("cell-selection-updated", null, function() {
	var cellSelUI = new cellSelectionUIcontroller();
	cellSelUI.syncCellSelectionStore();
    });

}

/**
 * Sync the cell selection store with the current selections
 */
cellSelectionUIcontroller.prototype.syncCellSelectionStore = function() {
    // Get the store for the table and empty it
    var store = Ext.data.StoreManager.lookup('cellSelectionStoreForSelectionTable');
    store.removeAll();

    // Repopulate the store
    var cellSelCntr =  new cellSelectionController();
    var availSelections = cellSelCntr.getAvailableSelections();
    
    for (sel in availSelections) {
	var selName = availSelections[sel];
	var selCount =  cellSelCntr.getSelection(selName).length;
	var selDisplayName =  cellSelCntr.getSelectionDisplayName(selName);

	store.add({selectionname: selName, displayname: selDisplayName , cellcount: selCount});
    }// for
}

/** 
 * Generate the cell selection store
 */
cellSelectionUIcontroller.prototype.generateCellSelectionStore = function() {
    Ext.create('Ext.data.Store', {
	storeId: 'cellSelectionStoreForSelectionTable',
	fields: [
	    {name: 'selectionname', type: 'string'},
	    {name: 'displayname', type: 'string'},
	    {name: 'cellcount', type: 'integer'}
	],
	autoLoad: true
    });
}

/**
 * Generate the User interface
 * @private
 */
cellSelectionUIcontroller.prototype.generateUI = function() {
    var uipanel = Ext.getCmp('cellselection-app-container');
    
    var cellSelectionTable = Ext.create('Ext.grid.Panel',{
	title: 'Available Cell Selections',
	id: 'cellSelectionTable',
	store: Ext.data.StoreManager.lookup('cellSelectionStoreForSelectionTable'),
	columns: [
	    {text: 'Name', dataIndex: 'displayname', width: '70%'},
	    {text: 'Count', dataIndex: 'cellcount', width: '29%'}
	],
	emptyText: "No cell selections are currently available"
    });
    

    var formPanel = Ext.create('Ext.form.Panel', {
    height: '100%',
    width: '100%',
    bodyPadding: 10,
    defaultType: 'textfield',
    items: [
	cellSelectionTable,

        {
            xtype: 'button',
            text: 'Duplicate',
	    handler: function() {
		var selectionTable = Ext.getCmp('cellSelectionTable');
		var selectedItems = selectionTable.getSelectionModel().getSelected();
		if (selectedItems.length === 1) {
		    var oldSelectionName = selectedItems.getAt(0).getData().selectionname;
		    var oldDisplayName = selectedItems.getAt(0).getData().displayname;
		    
		    Ext.MessageBox.prompt('New name', 'Name for new selection:',function(btn, text) {
			if ( btn === 'ok') {

			    var cellSelCntr =  new cellSelectionController();

			    var newSelectionName = text;
			    var newSelectionDisplayName = text;
			    
			    var re = new RegExp('[^A-Za-z0-9_]');
			    if (newSelectionName.length === 0) {
				Ext.MessageBox.alert('Error',
						     'You must enter a selection name');
			    }
			    else if (newSelectionName.match(re) ) {
				Ext.MessageBox.alert('Error', 
                                  'The name must only contain letters, numbers and underscores (_)');
			    } else {
				if (cellSelCntr.getSelection(newSelectionName)) {
				    Ext.MessageBox.alert(
					'Error',
					'A selection with this name already exists!');
				} else {
				    cellSelCntr.duplicateSelection(oldSelectionName,
								   newSelectionName,
								   newSelectionDisplayName);
				}

			    }
			}
		    });


		} else {
		    Ext.MessageBox.alert('Warning', 'Please choose a cell selection first');
		}
		
	    }
        },
	{
	    xtype: 'button',
	    text: 'Rename',
	    handler: function() {
		var selectionTable = Ext.getCmp('cellSelectionTable');
		var selectedItems = selectionTable.getSelectionModel().getSelected();
		if (selectedItems.length === 1) {
		    var oldSelectionName = selectedItems.getAt(0).getData().selectionname;

		    Ext.Msg.prompt('Rename Cell Selection', 'Please enter a new name:', function(btn, text) {
			if (btn =='ok') {
			    var newDisplayName = text;
			    var cellSelCntr =  new cellSelectionController();
			    cellSelCntr.renameSelection(oldSelectionName, newDisplayName);
			}
		    });

		} else {
		    Ext.MessageBox.alert('Warning', 'Please choose a cell selection first');
		}


	    }
	},
	{
	    xtype: 'button',
	    text: 'Export CSV',
	    handler: function() {
		var selectionTable = Ext.getCmp('cellSelectionTable');
		var selectedItems = selectionTable.getSelectionModel().getSelected();
		if (selectedItems.length === 1) {
		    var selectionName = selectedItems.getAt(0).getData().selectionname;
		    var cellSelCntr = new cellSelectionController();
		    var selection = cellSelCntr.getSelection(selectionName);		    
		    var selectionFormatted = selection.join("\n");
		    window.open('data:application/csv;charset=utf-8,' + encodeURI(selectionFormatted));

		} else {
		    Ext.MessageBox.alert('Warning', 'Please choose a cell selection first');
		}		    
	    }
	}
    ]
    });

    uipanel.add(formPanel);

}
   
