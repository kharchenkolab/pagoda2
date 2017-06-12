"use strict";
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

    for (var sel in availSelections) {
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

    var cellTableSelectionModel =  Ext.create('Ext.selection.CheckboxModel', {});
    var cellSelectionTable = Ext.create('Ext.grid.Panel',{
    	title: 'Available Cell Selections',
    	id: 'cellSelectionTable',
    	store: Ext.data.StoreManager.lookup('cellSelectionStoreForSelectionTable'),
    	columns: [
    	    {text: 'Name', dataIndex: 'displayname', width: '70%'},
    	    {text: 'Count', dataIndex: 'cellcount', width: '29%'}
    	],
    	emptyText: "No cell selections are currently available",
    	singleSelect: false,
    	selModel: cellTableSelectionModel
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
        text: 'Delete',
        handler: function() {
          var selectionTable = Ext.getCmp('cellSelectionTable');
      		var selectedItems = selectionTable.getSelectionModel().getSelected();
      		if (selectedItems.length === 1) {
      		    var selName = selectedItems.getAt(0).getData().selectionname;
        		   Ext.Msg.show({
                   title:'Delete Selection',
                   msg: 'Delete ' + selectedItems.getAt(0).getData().displayname + '?',
                   buttons:  Ext.Msg.OKCANCEL,
                   fn: function(btn, text) {
                     if (btn === 'ok') {
                       var cellSel = new cellSelectionController();
                       cellSel.deleteSelection(selName);
                     }
                   }
                });
      		} else if (selectedItems.length === 0) {
      		  Ext.MessageBox.alert('Warning', 'Please select a cell selection first');
      		} else {
      		  Ext.MessageBox.alert('Warning', 'Only one cell selection can be deleted at a time.');
      		}

        }

      },
      {
        xtype: 'button',
        text: 'Merge',
        handler: function() {
          var selectionTable = Ext.getCmp('cellSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length == 2) {
            var selectionA = selectedItems.getAt(0).getData().selectionname;
            var selectionB = selectedItems.getAt(1).getData().selectionname;

            Ext.MessageBox.prompt('New name', 'Name for new selection:',function(btn, text) {
              if ( btn === 'ok') {
                var cellSelCntr =  new cellSelectionController();

                var newSelectionName = text;
                var newSelectionDisplayName = text;

                var re = new RegExp('[^A-Za-z0-9_]');
                if (newSelectionName.length === 0) {
                  Ext.MessageBox.alert('Error', 'You must enter a selection name');
                }
                else if (newSelectionName.match(re) ) {
                  Ext.MessageBox.alert('Error', 'The name must only contain letters, numbers and underscores (_)');
                } else {
                if (cellSelCntr.getSelection(newSelectionName)) {
                  Ext.MessageBox.alert('Error','A selection with this name already exists!');
                } else {
                  cellSelCntr.mergeSelectionsIntoNew(selectionA, selectionB,
                    newSelectionName, newSelectionDisplayName);
                }
              } // if btn ok
            }
          });
    		  } else {
            Ext.MessageBox.alert('Warning', 'Please pick two cell selections to merge first.');
    		  }

        }

      },
      ///
   {
        xtype: 'button',
        text: 'Intersect',
        handler: function() {
          var selectionTable = Ext.getCmp('cellSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length == 2) {
            var selectionA = selectedItems.getAt(0).getData().selectionname;
            var selectionB = selectedItems.getAt(1).getData().selectionname;

            Ext.MessageBox.prompt('New name', 'Name for new selection:',function(btn, text) {
              if ( btn === 'ok') {
                var cellSelCntr =  new cellSelectionController();

                var newSelectionName = text;
                var newSelectionDisplayName = text;

                var re = new RegExp('[^A-Za-z0-9_]');
                if (newSelectionName.length === 0) {
                  Ext.MessageBox.alert('Error', 'You must enter a selection name');
                }
                else if (newSelectionName.match(re) ) {
                  Ext.MessageBox.alert('Error', 'The name must only contain letters, numbers and underscores (_)');
                } else {
                if (cellSelCntr.getSelection(newSelectionName)) {
                  Ext.MessageBox.alert('Error','A selection with this name already exists!');
                } else {
                  cellSelCntr.intersectSelectionsIntoNew(selectionA, selectionB,
                    newSelectionName, newSelectionDisplayName);
                }
              } // if btn ok
            }
          });
    		  } else {
            Ext.MessageBox.alert('Warning', 'Please pick two cell selections to intersect first.');
    		  }

        }

      },
      ///
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
		    Ext.MessageBox.alert('Warning', 'Please choose only one cell selection first');
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
		    Ext.MessageBox.alert('Warning', 'Please choose only one cell selection first');
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
    		    Ext.MessageBox.alert('Warning', 'Please choose only one cell selection first');
    		}
	    }
	},
	{
        xtype: 'button',
        text: 'Export Selected',
        handler: function(){
          var selectionTable = Ext.getCmp('cellSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length >= 1) {
            var selectionFormatted = [];
            var cellSelCntr = new cellSelectionController();
            for(var index = 0; index < selectedItems.length; index++){
      		    var selectionName = selectedItems.getAt(index).getData().selectionname;
    	  	    var selection = cellSelCntr.getSelection(selectionName);
      		    selectionFormatted.push(selectionName+ "," + selection.join(","));
      		  }
    		    window.open('data:application/csv;charset=utf-8,' + encodeURI(selectionFormatted.join("\n")));
    		  } else {
    		    Ext.MessageBox.alert('Warning', 'Please choose one or more cell selections first');
    		  }
        }
	},
	{
	  xtype: 'button',
	  text: 'Import Selections',
  	  handler: function(){
	     if(!Ext.getCmp('cellFileSelectionWindow')){
	      Ext.create('Ext.window.Window',{
	        title:'Cell File Selection',
	        id: 'cellFileSelectionWindow',
	        height:100,
	        width:500,
	        align:"center",
	        items:[
	         {
	            height: "12px",
	            html: '<input type="file" id="selectedCellFile"><br>'
	          },
	          {
	            xtype: 'button',
	            text: 'cancel',
  	          width:"10%",
	            height:"40%",
	            align: "center",
	            handler: function(){
	              Ext.getCmp('cellFileSelectionWindow').close();
	            }
	          },
  	        {
  	          xtype: 'button',
	            text: 'ok',
	            width:"10%",
	            height:"40%",
	            align: "center",
	            handler: function(){
	              var cellSelFile = document.getElementById("selectedCellFile").files[0];
	              var cellSelFileName = cellSelFile.name;
	              var reader = new FileReader();
	              reader.onload = function(progressEvent){
	                var lines = this.result.split("\n");
	                var cellSelCntrl = new cellSelectionController();
	                for(var line = 0; line < lines.length; line++){
	                  var selection = lines[line].split(",");
	                  var selName = selection.shift();
	                  if(cellSelCntrl.getSelection(selName)){
	                    selName = selName  + "~RecentlyLoaded"
	                  }
	                  cellSelCntrl.setSelection(selName,selection,selName,"loaded from " + cellSelFileName);
	                }
	                Ext.MessageBox.alert('info',lines.length + " selections were loaded from " + cellSelFileName)
	              };
	              reader.readAsText(cellSelFile);
	              
	              Ext.getCmp('cellFileSelectionWindow').close();
	            }
	          }
	        ],
	      }).show();
	    }
	    Ext.getCmp('cellFileSelectionWindow').focus();
	    
	  }
	},
	{
	  xtype: 'button',
	  text: 'Highlight',
	  handler: function() {
	    	var selectionTable = Ext.getCmp('cellSelectionTable');
    		var selectedItems = selectionTable.getSelectionModel().getSelected();
    		if (selectedItems.length === 1) {
            var selectionName = selectedItems.getAt(0).getData().selectionname;

            // Highlight on heatmap
            var heatV = new heatmapViewer();
            heatV.highlightCellSelectionByName(selectionName);
            pagHelpers.regC(72);

            // Highlight on embedding
            var embCntr = new embeddingViewer();
            embCntr.highlightSelectionByName(selectionName);

            // Highlight on Aspects
            var aspHeatView = new aspectHeatmapViewer();
            aspHeatView.highlightCellSelectionByName(selectionName);

            //Highlight on Metadata
            var metaView = new metaDataHeatmapViewer();
            metaView.highlightCellSelectionByName(selectionName);


    		} else {
    		    Ext.MessageBox.alert('Warning', 'Please choose only one cell selection first');
    		    pagHelpers.regC(88);
    		}
	  }
	}
    ]
    });

    uipanel.add(formPanel);

}

