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
    var thisViewer = this;

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
      		if (selectedItems.length >= 1) {
      		    var dispNames = [];
      		    var selNames = [];
      		    for(var i = 0; i < selectedItems.length; i++){
      		      selNames.push(selectedItems.getAt(i).getData().selectionname);
      		      dispNames.push(selectedItems.getAt(i).getData().displayname)
      		    }
        		   Ext.Msg.show({
                   title:'Delete Selection',
                   msg: "Would you like to delete the following selections? <br>" + dispNames.join("<br>"),
                   buttons:  Ext.Msg.OKCANCEL,
                   fn: function(btn, text) {
                     if (btn === 'ok') {
                       var geneSel = new geneSelectionController();
                       selNames.forEach(function(selName){
                         geneSel.deleteSelection(selName);
                       });
                     }
                   }
                });
      		} else {
      		  Ext.MessageBox.alert('Warning', 'Please select a gene selection first');
          }

        }

      },
      {
        xtype: 'button',
        text: 'Merge',
        handler: function() {
          var selectionTable = Ext.getCmp('geneSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length >= 2) {
    		    
            var selectionNames = [];
            for(var i = 0; i < selectedItems.length; i++){
              selectionNames.push(selectedItems.getAt(i).getData().selectionname);
    		    }
            thisViewer.promptName("", function(newSelectionDisplayName){
              if(newSelectionDisplayName){
                var geneSelCntr = new geneSelectionController();
                geneSelCntr.mergeSelectionsIntoNew(selectionNames, newSelectionDisplayName);
              }
            })
            
    		  } else {
            Ext.MessageBox.alert('Warning', 'Please pick at least two gene selections to merge first.');
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
    		  if (selectedItems.length >= 2) {
      		    var selNames = [];
      		    for(var i = 0; i < selectedItems.length; i++){
      		      selNames.push(selectedItems.getAt(i).getData().selectionname);
      		    }
              thisViewer.promptName("", function(newSelectionDisplayName){
                if(newSelectionDisplayName){
                  var geneSelCntr = new geneSelectionController();
                  geneSelCntr.intersectSelectionsIntoNew(selNames, newSelectionDisplayName);
                }
              });
    		  } else {
              Ext.MessageBox.alert('Warning', 'Please pick at least two gene selection to intersect first.');
    		  }

        }

      },
	    {
		xtype: 'button',
		text: 'Save As',
		handler: function() {

		    var selectionTable = Ext.getCmp('geneSelectionTable');
		    var selectedItems = selectionTable.getSelectionModel().getSelected();

		    if (selectedItems.length === 1) {

			var oldSelectionName = selectedItems.getAt(0).getData().selectionname;
			var oldDisplayName = selectedItems.getAt(0).getData().displayname;
			var geneSelCntr = new geneSelectionController();
			 thisViewer.promptName("", function(newSelectionDisplayName){
          if(newSelectionDisplayName){
			      geneSelCntr.duplicateSelection(oldSelectionName,newSelectionDisplayName);
          }
			 });
			 
		    } else {
			     Ext.MessageBox.alert('Warning', 'Please choose a single gene selection first');
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
      		    var selectionName = selectedItems.getAt(index).getData().displayname;
    	  	    var selection = geneSelCntr.getSelection(selectedItems.getAt(index).getData().selectionname).genes;
      		    selectionFormatted.push(selectionName+ "," + selection.join(","));
      		  }
    		    window.open('data:application/csv;charset=utf-8,' + encodeURI(selectionFormatted.join("\n")));
    		  } else {
    		    Ext.MessageBox.alert('Warning', 'Please choose one or more gene selections first');
    		  }
        }
	    },
	    {
	  xtype: 'button',
	  text: 'Import Selections',
  	  handler: function(){
	     if(!Ext.getCmp('geneFileSelectionWindow')){
	      Ext.create('Ext.window.Window',{
	        title:'Gene File Selection',
	        id: 'geneFileSelectionWindow',
	        height:100,
	        width:500,
	        align:"center",
	        modal: true,
	        items:[
	          {
	            height: "12px",
	            html: '<input type="file" id="selectedGeneFile"><br>'
	          },
  	        {
  	          xtype: 'button',
	            text: 'Ok',
	            width:"10%",
	            height:"30%",
	            margin:"5 5 5 5",
	            align: "center",
	            handler: function(){
	              
	              var geneSelFile = document.getElementById("selectedGeneFile").files[0];
	              var geneSelFileName = geneSelFile.name;
	              var reader = new FileReader();
	              reader.onload = function(progressEvent){
	                var lines = this.result.split("\n");
	                var dataCntr = new dataController();
	                dataCntr.getGeneInformationStore(function(geneInformationStore){
	                var geneOrder = [];
                  for(var i = 0; i < geneInformationStore.localData.length; i++){
                      geneOrder.push(geneInformationStore.localData[i].genename)
                  }
	                var geneSelCntrl = new geneSelectionController();
	                var total = 0;
	                var removedGenes = {};
	                for(var line = 0; line < lines.length; line++){
	                  if(lines[line].length !== 0){
	                    var selection = lines[line].split(",");
	                    var dispName = selection.shift();
	                    removedGenes[dispName] = 0;
	                    var pureSelection = [];
	                    for(var elem = 0; elem < selection.length; elem++){
	                      if(geneOrder.includes(selection[elem])){
	                        pureSelection.push(selection[elem]);
	                      }
	                      else{
	                        removedGenes[dispName]++;
	                      }
	                    }// ensure all genes are rightfully containers
	                    
	                    
	                    if(removedGenes[dispName] !== selection.length){
	                      while(geneSelCntrl.displayNameExists(dispName)){
  	                      dispName = dispName  + "~RecentlyLoaded"
	                      }
	                      geneSelCntrl.setSelection(pureSelection,dispName);
	                      total++;
	                    }//confirm
	                  }
	                }
	                var extraInfo = "";
	                for(var selName in removedGenes){
	                  if(removedGenes[selName] > 0){
	                    extraInfo += "<br>" + removedGenes[selName] + " gene(s) could not be loaded from selection " + selName;
	                  }
	                }
	                Ext.MessageBox.alert('Load Gene Selections Complete', total + " selections were generated from the data within " + geneSelFileName + extraInfo)
	                });
  	           };
	             reader.readAsText(geneSelFile);
	             Ext.getCmp('geneFileSelectionWindow').close();
	            }
	          },
	          {
	            xtype: 'button',
	            text: 'Cancel',
  	          width:"10%",
	            height:"30%",
	            margin: "5 5 5 5",
	            align: "center",
	            handler: function(){
	              Ext.getCmp('geneFileSelectionWindow').close();
	            }
	          },
	        ],
	      }).show();
	    }
	    Ext.getCmp('geneFileSelectionWindow').focus();
	    
	  }
	},
	]
    });

    uipanel.add(formPanel);
}

geneSelectionUIcontroller.prototype.promptName = function(curDisplay, callback){
    		    Ext.Msg.prompt('Rename Gene Selection', 'Please enter a new name:', function(btn, text) {
        			if (btn =='ok') {
      			    var newName = text;
      			    var geneSelCntr = new geneSelectionController();
      			    var geneSelUICntr = new geneSelectionUIcontroller();
        			 var re = new RegExp('[^A-Za-z0-9_]');
      			    if (newName.length === 0) {
      				    Ext.MessageBox.alert('Error','You must enter a selection name',function(e){
        			      geneSelUICntr.promptName(newName, callback);
        			    });
      			    }
      			    else if (newName.match(re) ) {
      				    Ext.MessageBox.alert('Error', 'The name must only contain letters, numbers and underscores (_)',function(e){
        			      geneSelUICntr.promptName(newName, callback);
        			    });
      			    } 
      			    else if (geneSelCntr.displayNameExists(newName)) {
      				    Ext.MessageBox.alert('Error', 'A selection with this name already exists!',function(e){
        			      geneSelUICntr.promptName(newName, callback);
        			    });
      				  }
      				  else{
      				    callback(newName)
      				  }
        			}
        			else{
        			  callback(false);
        			}
    		    },{},false,curDisplay);
  
}
