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
    //this.generateToolbar();
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
    var toolbar = this.generateToolbar();




    var geneSelectionTable = Ext.create('Ext.grid.Panel', {
	title: 'Gene Selections',
	tools: [toolbar],
	id: 'geneSelectionTable',
	height: '100%',
	width: '100%',
	store: Ext.data.StoreManager.lookup('geneSelectionStoreForSelectionTable'),
	columns: [
	    {text: 'Name', dataIndex: 'displayname', width: '70%'},
	    {text: 'Count', dataIndex: 'cellcount', width: '29%'}
	],
	emptyText: 'No gene selections are currently available',
	selModel: geneTableSelectionModel,
    });

    uipanel.add(geneSelectionTable);

}

/**
 * Generate the user interface for selecting a new name. Blocks until proper name given or user cancles request
 * @param {curDisplay} Information that appears in text field at the beginning
 * @param {callback}
 */
geneSelectionUIcontroller.prototype.promptName = function(curDisplay, callback){
    		    Ext.Msg.prompt('Rename Gene Selection', 'Please enter a new name:', function(btn, text) {
        			if (btn =='ok') {
      			    var newName = text;
      			    var geneSelCntr = new geneSelectionController();
      			    var geneSelUICntr = new geneSelectionUIcontroller();
        			 var re = new RegExp(',');
      			    if (newName.length === 0) {
      				    Ext.MessageBox.alert('Error','You must enter a selection name',function(e){
        			      geneSelUICntr.promptName(newName, callback);
        			    });
      			    }
      			    else if (newName.match(re) ) {
      				    Ext.MessageBox.alert('Error', 'The name must not contain a comma',function(e){
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

/**
 * Builds the UI toolbar for the gene selection window
 */
geneSelectionUIcontroller.prototype.generateToolbar = function(){
  var toolbar = Ext.create("Ext.Toolbar");
  var thisViewer = this;

  toolbar.add({
        xtype: 'button',
       glyph: 0xf055, //fa-plus-circle
        tooltip: 'Merge',
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

      });
  toolbar.add({
	  xtype: "button",
	  glyph: 0xf056, //fa-minus-circle
	  tooltip: "Difference",
	  handler: function(){
	    var selectionTable = Ext.getCmp('geneSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length >= 2) {
            var selectionNames = [];
            for(var i = 0; i < selectedItems.length; i++){
              selectionNames.push(selectedItems.getAt(i).getData().selectionname);
    		    }
            thisViewer.promptName("", function(newDisplayName){
          	  var geneSelCntr =  new geneSelectionController();
          	  if(newDisplayName !== false){
          	    geneSelCntr.differenceSelectionsIntoNew(selectionNames, newDisplayName);
      	      }
            })


    		  } else {
            Ext.MessageBox.alert('Warning', 'Please pick at least two gene selections to find the difference of first.');
    		  }
	  }
	});
  toolbar.add({
    xtype: 'button',
   glyph: 0xf042, //fa-adjust
    tooltip: 'Intersect',
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
  });
  toolbar.add({
	  xtype: "button",
	  glyph: 0xf057, //fa-stop-circle
	  tooltip: "Compliment",
	  handler: function(){
	    var selectionTable = Ext.getCmp('geneSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length >= 1) {
            var selectionNames = [];
            for(var i = 0; i < selectedItems.length; i++){
              selectionNames.push(selectedItems.getAt(i).getData().selectionname);
    		    }
            thisViewer.promptName("", function(newDisplayName){
          	  var geneSelCntr =  new geneSelectionController();
          	  if(newDisplayName !== false){
          	    geneSelCntr.complimentSelectionsIntoNew(selectionNames, newDisplayName);
      	      }
            })


    		  } else {
            Ext.MessageBox.alert('Warning', 'Please pick at least one gene selection to compliment first.');
    		  }
	  }
	});
	toolbar.add({xtype: 'tbseparator'});
	toolbar.add({
      xtype: 'button',
      glyph: 0xf24d , //fa-clone
      tooltip: 'Copy',
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
		}
        });
  toolbar.add({
	    xtype: 'button',
	    glyph: 0xf246, //fa-I-cursor
	    tooltip: 'Rename',
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

     }
	});
	toolbar.add({
        xtype: 'button',
        glyph: 0xf1f8, // fa-trash
        tooltip: 'Delete',
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
      });
  toolbar.add({
        xtype: 'button',
        glyph: 0xf0db,
        tooltip: 'Display',
        handler: function() {
          var selectionTable = Ext.getCmp('geneSelectionTable');
      		var selectedItems = selectionTable.getSelectionModel().getSelected();
      		if (selectedItems.length >= 1) {
      		    var dispNames = [];
      		    var selNames = [];
      		    for(var i = 0; i < selectedItems.length; i++){
      		      selNames.push(selectedItems.getAt(i).getData().selectionname);
      		    }
      		    (new geneSelectionTableViewer()).generateTableFromSelection(selNames);
      		} else {
      		  Ext.MessageBox.alert('Warning', 'Please select a gene selection first');
          }

        }
      });
  toolbar.add({xtype: 'tbseparator'});
  toolbar.add({
        xtype: 'button',
        glyph: 0xf0c7 , //fa-floppy-o
        tooltip: 'Export Selected',
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

	});
	toolbar.add({
	  xtype: 'button',
	  glyph: 0xf115, //fa-folder-open-o
	  tooltip: 'Import Selections',
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
	                var reader = this;
	                var dataCntr = new dataController();
	                dataCntr.getGeneInformationStore(function(geneInformationStore){
	                var params = {}
	                params.lines = reader.result.split("\n");
	                params.geneOrder = [];
	                params.total = 0;
	                params.removedGenes = {};
	                params.geneSelCntrl = new geneSelectionController();
	                for(var i = 0; i< params.lines.length; i++){
	                  while(params.lines[i].length === 0 && i < params.lines.length){
	                    params.lines.splice(i,1)
	                  }
	                }

                  for(var i = 0; i < geneInformationStore.localData.length; i++){
                      params.geneOrder.push(geneInformationStore.localData[i].genename)
                  }

	                var functionStep = function(params,i, step, max){
	                  for(var line = 0; line < step; line++){
	                    var selection = params.lines[i + line].split(",");

	                    var dispName = selection.shift();
	                    params.removedGenes[dispName] = 0;
	                    var pureSelection = [];
	                    for(var elem = 0; elem < selection.length; elem++){
	                      if(params.geneOrder.includes(selection[elem])){
	                        pureSelection.push(selection[elem]);
	                      }
	                      else{
	                        params.removedGenes[dispName]++;
	                      }
	                    }// ensure all genes are rightfully containers


	                    if(params.removedGenes[dispName] !== selection.length){
	                      while(params.geneSelCntrl.displayNameExists(dispName)){
  	                      dispName = dispName  + "~RecentlyLoaded"
	                      }
	                      params.geneSelCntrl.setSelection(pureSelection,dispName);
	                      params.total++;
	                    }//confirm
	                  }
	                }

	                var callback = function(params){
	                  var extraInfo = "";
  	                for(var selName in params.removedGenes){
  	                  if(params.removedGenes[selName] > 0){
  	                    extraInfo += "<br>" + params.removedGenes[selName] + " gene(s) could not be loaded from selection " + selName;
  	                  }
  	                }
  	                Ext.MessageBox.alert('Load Gene Selections Complete', params.total + " selections were generated from the data within " + geneSelFileName + extraInfo)
  	               }
  	               pagHelpers.generateProgressBar(functionStep,params.lines.length,1,callback,params);
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
	});

	return toolbar;
}
