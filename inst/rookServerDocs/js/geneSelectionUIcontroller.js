"use strict";
/**
 * Manages the gene selection interface
 * @constructor
 */
function geneSelectionUIcontroller() {
    if (typeof geneSelectionUIcontroller.instance === 'object' ) {
	return geneSelectionUIcontroller.instance
    }

    geneSelectionUIcontroller.instance = this;

    this.generateGeneSelectionStore();
    this.generateUI();

    var evtBus = new eventBus();
    evtBus.register('gene-selection-updated', null, function() {
	var geneSelUI = new geneSelectionUIcontroller();
	geneSelUI.syncGeneSelectionStore();
    });
}

/**
 * Sync the display store with the selection controller
 */
geneSelectionUIcontroller.prototype.syncGeneSelectionStore = function() {
    var store = Ext.data.StoreManager.lookup('geneSelectionStoreForSelectionTable');
    store.removeAll();

    // Repopulate
    var geneSelCntr = new geneSelectionController();
    var availSelections = geneSelCntr.getAvailableSelections();

    for (var sel in availSelections) {
	var selName = availSelections[sel];
	var selCount = geneSelCntr.getSelection(selName).genes.length;
	var selDisplayName = geneSelCntr.getSelectionDisplayName(selName);

	store.add({
	    selectionname: selName,
	    displayname: selDisplayName,
	    cellcount: selCount
	});
    } // for

}

/**
 * Make and register the store for the table
 */
geneSelectionUIcontroller.prototype.generateGeneSelectionStore = function() {
    Ext.create('Ext.data.Store', {
	storeId:  'geneSelectionStoreForSelectionTable',
	fields: [
	    {name: 'selectionname', type: 'string'},
	    {name: 'displayname', type: 'string'},
	    {name: 'cellcount', type: 'integer'}
	],
	autoLoad: true
    });
}

/**
 * Generate the user interface, assumes existence
 * of 'geneselection-app-container' extjs component
 */
geneSelectionUIcontroller.prototype.generateUI = function() {
    var uipanel = Ext.getCmp('geneselection-app-container');
    var geneTableSelectionModel =  Ext.create('Ext.selection.CheckboxModel', {});

    var geneSelectionTable = Ext.create('Ext.grid.Panel', {
	title: 'Available Gene Selections',
	id: 'geneSelectionTable',
	store: Ext.data.StoreManager.lookup('geneSelectionStoreForSelectionTable'),
	columns: [
	    {text: 'Name', dataIndex: 'displayname', width: '70%'},
	    {text: 'Count', dataIndex: 'cellcount', width: '29%'}
	],
	emptyText: 'No gene selections are currently available',
	selModel: geneTableSelectionModel
    });

    var formPanel = Ext.create('Ext.form.Panel', {
	height: '100%',
	width: '100%',
	bodyPadding: 10,
	defaultType: 'textfield',
	items: [
	    geneSelectionTable,
	    {
        xtype: 'button',
        text: 'Delete',
        handler: function() {
          var selectionTable = Ext.getCmp('geneSelectionTable');
      		var selectedItems = selectionTable.getSelectionModel().getSelected();
      		if (selectedItems.length === 1) {
      		    var selName = selectedItems.getAt(0).getData().selectionname;
        		   Ext.Msg.show({
                   title:'Delete Selection',
                   msg: 'Delete ' + selectedItems.getAt(0).getData().displayname + '?',
                   buttons:  Ext.Msg.OKCANCEL,
                   fn: function(btn, text) {
                     if (btn === 'ok') {
                       var geneSel = new geneSelectionController();
                       geneSel.deleteSelection(selName);
                     }
                   }
                });
      		} else if (selectedItems.length === 0) {
      		  Ext.MessageBox.alert('Warning', 'Please select a gene selection first');
      		} else {
      		  Ext.MessageBox.alert('Warning', 'Only one gene selection can be deleted at a time.');
      		}

        }

      },
      {
        xtype: 'button',
        text: 'Merge',
        handler: function() {
          var selectionTable = Ext.getCmp('geneSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length == 2) {
            var selectionA = selectedItems.getAt(0).getData().selectionname;
            var selectionB = selectedItems.getAt(1).getData().selectionname;

            Ext.MessageBox.prompt('New name', 'Name for new selection:',function(btn, text) {
              if ( btn === 'ok') {
                var geneSelCntr =  new geneSelectionController();

                var newSelectionName = text;
                var newSelectionDisplayName = text;

                var re = new RegExp('[^A-Za-z0-9_]');
                if (newSelectionName.length === 0) {
                  Ext.MessageBox.alert('Error', 'You must enter a selection name');
                }
                else if (newSelectionName.match(re) ) {
                  Ext.MessageBox.alert('Error', 'The name must only contain letters, numbers and underscores (_)');
                } else {
                if (geneSelCntr.getSelection(newSelectionName)) {
                  Ext.MessageBox.alert('Error','A selection with this name already exists!');
                } else {
                  geneSelCntr.mergeSelectionsIntoNew(selectionA, selectionB,
                    newSelectionName, newSelectionDisplayName);
                }
              } // if btn ok
            }
          });
    		  } else {
            Ext.MessageBox.alert('Warning', 'Please pick two gene selection to merge first.');
    		  }

        }

      },
      ///
   {
        xtype: 'button',
        text: 'Intersect',
        handler: function() {
          var selectionTable = Ext.getCmp('geneSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length == 2) {
            var selectionA = selectedItems.getAt(0).getData().selectionname;
            var selectionB = selectedItems.getAt(1).getData().selectionname;

            Ext.MessageBox.prompt('New name', 'Name for new selection:',function(btn, text) {
              if ( btn === 'ok') {
                var geneSelCntr =  new geneSelectionController();

                var newSelectionName = text;
                var newSelectionDisplayName = text;

                var re = new RegExp('[^A-Za-z0-9_]');
                if (newSelectionName.length === 0) {
                  Ext.MessageBox.alert('Error', 'You must enter a selection name');
                }
                else if (newSelectionName.match(re) ) {
                  Ext.MessageBox.alert('Error', 'The name must only contain letters, numbers and underscores (_)');
                } else {
                if (geneSelCntr.getSelection(newSelectionName)) {
                  Ext.MessageBox.alert('Error','A selection with this name already exists!');
                } else {
                  geneSelCntr.intersectSelectionsIntoNew(selectionA, selectionB,
                    newSelectionName, newSelectionDisplayName);
                }
              } // if btn ok
            }
          });
    		  } else {
            Ext.MessageBox.alert('Warning', 'Please pick two gene selection to intersect first.');
    		  }

        }

      },
	    {
		xtype: 'button',
		text: 'Duplicate',
		handler: function() {

		    var selectionTable = Ext.getCmp('geneSelectionTable');
		    var selectedItems = selectionTable.getSelectionModel().getSelected();

		    if (selectedItems.length === 1) {

			var oldSelectionName = selectedItems.getAt(0).getData().selectionname;
			var oldDisplayName = selectedItems.getAt(0).getData().displayname;

			Ext.MessageBox.prompt('New name', 'Name for new selection:',
					      function(btn, text) {
						  if (btn === 'ok')  {
						      var geneSelCntr = new geneSelectionController();

						      var newSelectionName = text;
						      var newSelectionDisplayName = text;

						      var re = new RegExp('[^A-Za-z0-9_]');
						      if (newSelectionName.length === 0) {
							  Ext.MessageBox.alert('Error', 'You must enter a selection name');
						      } else if ( newSelectionName.match(re) ) {
							  Ext.MessageBox.alert('Error',
									       'The name must only contain letters, numbers and underscores (_)');
						      } else {
							  if (geneSelCntr.getSelection(newSelectionName)) {
							      Ext.MessageBox.alert(
								  'Error',
								  'A selection with this name already exists!');
							  } else {
							      geneSelCntr.duplicateSelection(oldSelectionName,
											     newSelectionName,
											     newSelectionDisplayName);
							  }
						      } // if lenth == 0
						  } // if btn == ok
					      }); // MessageBox.prompt
		    } else {
			Ext.MessageBox.alert('Warning', 'Please choose a gene selection first');
		    } // selectedItems == 1
		} // handler
	    },
	    {
		xtype: 'button',
		text: 'Rename',
		handler: function() {
		    var selectionTable = Ext.getCmp('geneSelectionTable');
		    var selectedItems =  selectionTable.getSelectionModel().getSelected();
		    if (selectedItems.length === 1) {
			var oldSelectionName = selectedItems.getAt(0).getData().selectionname;
			Ext.Msg.prompt('Rename Gene Selection', 'Please enter a new name:',
				       function(btn, text) {
					   if (btn === 'ok') {
					       var newDisplayName = text;
					       var geneSelCntr = new geneSelectionController();
					       geneSelCntr.renameSelection(oldSelectionName, newDisplayName);
					   } // btn === ok
				       });
		    } else {
			Ext.MessageBox.alert('Warning', 'Please choose a gene selection first');
		    } // if legnth == 1


                } // Rename handler
	    },
	    {
		xtype: 'button',
		text: 'Export CSV',
		handler: function() {
		    var selectionTable = Ext.getCmp('geneSelectionTable');
		    var selectedItems = selectionTable.getSelectionModel().getSelected();
		    if (selectedItems.length === 1) {
			    var selectionName = selectedItems.getAt(0).getData().selectionname;
			    var geneSelCntr = new geneSelectionController();
			    var selection = geneSelCntr.getSelection(selectionName).genes;
			    var selectionFormatted = selection.join("\n");
			    window.open('data:application/csv;charset=utf-8,' + encodeURI(selectionFormatted));
		    } else  {
		  	  Ext.MessageBox.alert('Warning', 'Please choose a gene selection first');
		    }
		}
	    },
	    {
        xtype: 'button',
        text: 'Export Selected',
        handler: function(){
          var selectionTable = Ext.getCmp('geneSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length >= 1) {
            var selectionFormatted = [];
            var geneSelCntr = new geneSelectionController();
            for(var index = 0; index < selectedItems.length; index++){
      		    var selectionName = selectedItems.getAt(index).getData().selectionname;
    	  	    var selection = geneSelCntr.getSelection(selectionName).genes;
    	  	    console.log(selection)
      		    selectionFormatted.push(selectionName+ "," + selection.join(","));
      		  }
    		    window.open('data:application/csv;charset=utf-8,' + encodeURI(selectionFormatted.join("\n")));
    		  } else {
    		    Ext.MessageBox.alert('Warning', 'Please choose one or more gene selections first');
    		  }
        }
	    }
	]
    });

    uipanel.add(formPanel);
}
