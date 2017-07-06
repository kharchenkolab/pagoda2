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
	var selColor = cellSelCntr.getColor(selName);

	store.add({selectionname: selName, displayname: selDisplayName , cellcount: selCount, color: selColor});
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

    var thisViewer = this;
    var cellTableSelectionModel =  Ext.create('Ext.selection.CheckboxModel', {});
    var cellSelectionTable = Ext.create('Ext.grid.Panel',{
    	title: 'Available Cell Selections',
    	id: 'cellSelectionTable',
    	store: Ext.data.StoreManager.lookup('cellSelectionStoreForSelectionTable'),
    	columns: [
    	    {text: 'Name', dataIndex: 'displayname', width: '62%'},
    	    {text: 'Count', dataIndex: 'cellcount', width: '28%'},
    	    {text: "&#x03DF;", dataIndex: 'color',width:'5%', renderer:
    	    function(value, meta){
    	      meta.style = "background-color:"+value+";";
    	    }}
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
                       var cellSel = new cellSelectionController();
                       selNames.forEach(function(selName){
                         cellSel.deleteSelection(selName);
                       });
                     }
                   }
                });
      		} else {
      		  Ext.MessageBox.alert('Warning', 'Please select a cell selection first');

          }
        }
      },
  {
        xtype: 'button',
        text: 'Merge',
        handler: function() {
          var selectionTable = Ext.getCmp('cellSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length >= 2) {

    		    var selectionNames = [];
            for(var i = 0; i < selectedItems.length; i++){
              selectionNames.push(selectedItems.getAt(i).getData().selectionname);
    		    }

    		    thisViewer.promptName("", function(newDisplayName){
          	  var cellSelCntr =  new cellSelectionController();
          	  if(newDisplayName !== false){
          	    cellSelCntr.mergeSelectionsIntoNew(selectionNames, newDisplayName);
      	    }
      	})

    		  } else {
            Ext.MessageBox.alert('Warning', 'Please pick at least two cell selections to merge first.');
    		  }

        }

      },
  {
        xtype: 'button',
        text: 'Intersect',
        handler: function() {
          var selectionTable = Ext.getCmp('cellSelectionTable');
    		  var selectedItems = selectionTable.getSelectionModel().getSelected();
    		  if (selectedItems.length >= 2) {
            var selectionNames = [];
            for(var i = 0; i < selectedItems.length; i++){
              selectionNames.push(selectedItems.getAt(i).getData().selectionname);
    		    }
            thisViewer.promptName("", function(newDisplayName){
          	  var cellSelCntr =  new cellSelectionController();
          	  if(newDisplayName !== false){
          	    cellSelCntr.intersectSelectionsIntoNew(selectionNames, newDisplayName);
      	      }
            })


    		  } else {
            Ext.MessageBox.alert('Warning', 'Please pick at least two cell selections to intersect first.');
    		  }

        }

      },
  {
            xtype: 'button',
            text: 'Save As',
	    handler: function() {
    		var selectionTable = Ext.getCmp('cellSelectionTable');
    		var selectedItems = selectionTable.getSelectionModel().getSelected();
    		if (selectedItems.length === 1) {
    		    var oldSelectionName = selectedItems.getAt(0).getData().selectionname;
    		    var oldDisplayName = selectedItems.getAt(0).getData().displayname;

            thisViewer.promptName(oldDisplayName, function(newDisplayName){
          	  var cellSelCntr =  new cellSelectionController();
          	  if(newDisplayName !== false){
          	    cellSelCntr.duplicateSelection(oldSelectionName,newDisplayName);
          	  }
          	})


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
		    var oldDisplayName = selectedItems.getAt(0).getData().displayName;
		    var oldSelectionName = selectedItems.getAt(0).getData().selectionname;
      	thisViewer.promptName(oldDisplayName, function(newDisplayName){
      	  var cellSelCntr =  new cellSelectionController();
      	  if(newDisplayName !== false){
      	    cellSelCntr.renameSelection(oldSelectionName, newDisplayName);
      	  }
      	})
		} else {
		    Ext.MessageBox.alert('Warning', 'Please choose only one cell selection first');
		}
	    }
	},
	{
        xtype: 'button',
        text: 'Export Selected',
        handler: function(){
      var importOptionsStore = Ext.create('Ext.data.Store', {
	       fields: ['label', 'value'],
	       id: 'importOptionsStore'
      });
      importOptionsStore.add({
        label: "Pagoda CSV",
        value: "csv"
      });
      /*importOptionsStore.add({
        label: "JSON format",
        value: "json"
      });*/

      // Make a combobox
      var importComboBox = Ext.create('Ext.form.ComboBox', {
	      fieldLabel: 'Export Format:',
	      store: importOptionsStore,
	      queryMode: 'local',
	      displayField: 'label',
	      editable: false,
	      valueField: 'value',
	      required: true,
        columnWidth: "100%",
        margin: "5 5 5 5",
      });
      var selectionTable = Ext.getCmp('cellSelectionTable');
		  var selectedItems = selectionTable.getSelectionModel().getSelected();
	    if (selectedItems.length >= 1) {
	      Ext.create('Ext.window.Window',{
	        title:'Cell File Selection',
	        id: 'cellFileCreationWindow',
	        height:100,
	        width:300,
	        align:"center",
	        modal: true,
	        resizeable: false,
	        items:[
            importComboBox,
            {
  	          xtype: 'button',
	            text: 'Ok',
	            width:"45px",
	            height:"25px",
	            columnWidth: "20%",
	            margin: "5 5 5 5",
	            align: "center",
	            handler: function(){

	              if(importComboBox.getValue() === "csv"){
                  var selectionFormatted = [];
                  var cellSelCntr = new cellSelectionController();
                  for(var index = 0; index < selectedItems.length; index++){
                    var selectionName = selectedItems.getAt(index).getData().selectionname;
      	            var displayName = selectedItems.getAt(index).getData().displayname;
      	            var color = selectedItems.getAt(index).getData().color.substring(1);
    	              var selection = cellSelCntr.getSelection(selectionName);
                    selectionFormatted.push(color + "," + displayName + "," + selection.join(","));
  		            }
  		              window.open('data:application/csv;charset=utf-8,' + encodeURI(selectionFormatted.join("\n")));
                }
	              else if(importComboBox.getValue() === "json"){
	                    Ext.Msg.alert("Warning", "File format not yet supported");
	              }
	              else{
	                    Ext.Msg.alert("Error", "No file format specified");
	                    Ext.getCmp('cellFileCreationWindow').close();
	                    return;
	              }
   	            Ext.getCmp('cellFileCreationWindow').close();
	            }
	          },
	          {
	            xtype: 'button',
	            text: 'Cancel',
  	          width:"70px",
	            height:"25px",
	            columnWidth: "20%",
	            margin: "5 5 5 5",
	            align: "center",
	            handler: function(){
	              Ext.getCmp('cellFileCreationWindow').close();
	            }
	          },
	        ],
	      }).show();

	    } else {
    		Ext.MessageBox.alert('Warning', 'Please choose one or more cell selections first');
    	}
    }

	},
	{
	  xtype: 'button',
	  text: 'Import Selections',
  	  handler: function(){
  	    // Define a store for the options
      var importOptionsStore = Ext.create('Ext.data.Store', {
	       fields: ['label', 'value'],
	       id: 'importOptionsStore'
      });
      importOptionsStore.add({
        label: "Pagoda CSV",
        value: "csv"
      });
      /*importOptionsStore.add({
        label: "JSON format",
        value: "json"
      });*/

      // Make a combobox
      var importComboBox = Ext.create('Ext.form.ComboBox', {
	      fieldLabel: 'Import Format:',
	      store: importOptionsStore,
	      queryMode: 'local',
	      id: 'importComboBox',
	      displayField: 'label',
	      editable: false,
	      valueField: 'value',
	      required: true,
        columnWidth: "100%",
        margin: "5 5 5 5",
      });
	    if(!Ext.getCmp('cellFileSelectionWindow')){
	      Ext.create('Ext.window.Window',{
	        title:'Cell File Selection',
	        id: 'cellFileSelectionWindow',
	        height:100,
	        width:500,
	        align:"center",
	        modal: true,
	        resizeable: false,
	        items:[
	          {
	            height: "12px",
	            html: '<input type="file" id="selectedCellFile"><br>'
	          },
	          Ext.create('Ext.panel.Panel', {
              layout : 'hbox' ,
              items: [
            {
  	          xtype: 'button',
	            text: 'Ok',
	            width:"45px",
	            height:"25px",
	            columnWidth: "20%",
	            margin: "5 5 5 5",
	            align: "center",
	            handler: function(){
	              var dataCntr = new dataController();
	              var cellSelFile = document.getElementById("selectedCellFile").files[0];
	              var cellSelFileName = cellSelFile.name;
	              var reader = new FileReader();
	              var selection = Ext.getCmp("importComboBox").getValue()
	              if(selection === "csv"){
	                reader.onload = function(progressEvent){
	                  var lines = this.result.split("\n");
  	                dataCntr.getCellOrder(function(cellOrder){
  	                  var cellSelCntrl = new cellSelectionController();
	                    var total = 0;
	                    var removedCells = {};

  	                  for(var line = 0; line < lines.length; line++){
	                      if(lines[line].length !== 0){
	                        var selection = lines[line].split(",");
	                        var color = "#" + selection.shift();
	                        var dispName = selection.shift();
	                        removedCells[dispName] = 0;
	                        var pureSelection = [];
	                        for(var elem = 0; elem < selection.length; elem++){
	                          if(cellOrder.includes(selection[elem])){
	                            pureSelection.push(selection[elem]);
	                          }
  	                        else{
	                            removedCells[dispName]++;
	                          }
  	                      }// ensure all cells are rightfully containers

	                        if(removedCells[dispName] !== selection.length){
	                          while(cellSelCntrl.displayNameExists(dispName)){
  	                          dispName = dispName  + "~RecentlyLoaded"
	                          }
	                          cellSelCntrl.setSelection(pureSelection,dispName,{}, color);
	                          total++;
	                        }//confirm
  	                    }
	                    }
	                  var extraInfo = "";
	                  for(var selName in removedCells){
	                    if(removedCells[selName] > 0){
	                      extraInfo += "<br>" + removedCells[selName] + " cell(s) could not be loaded from selection " + selName;
	                    }
	                  }
	                    Ext.MessageBox.alert('Load Cell Selections Complete', total + " selections were generated from the data within " + cellSelFileName + extraInfo)
	                  });
  	           };
	             }
	             else if(selection === "json"){

	             }
	             else if(selection === "pagbin"){

	             }
	             else{
	               Ext.Msg.alert("Error", "An unexpected error has occured");
                 return;
	             }
	             reader.readAsText(cellSelFile);
	             Ext.getCmp('cellFileSelectionWindow').close();
	            }
	          },
	          {
	            xtype: 'button',
	            text: 'Cancel',
  	          width:"70px",
	            height:"25px",
	            columnWidth: "20%",
	            margin: "5 5 5 5",
	            align: "center",
	            handler: function(){
	              Ext.getCmp('cellFileSelectionWindow').close();
	            }
	          },
	          importComboBox

            ]
            })
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

    		    var selectionNames = []
    		    for(var i = 0; i < selectedItems.length; i++){
              selectionNames.push(selectedItems.getAt(i).getData().selectionname);
    		    }

            // Highlight on heatmap
            var heatV = new heatmapViewer();
            heatV.highlightCellSelectionsByNames(selectionNames);
            pagHelpers.regC(72);

            // Highlight on embedding
            var embCntr = new embeddingViewer();
            embCntr.highlightSelectionsByNames(selectionNames);

            // Highlight on Aspects
            var aspHeatView = new aspectHeatmapViewer();
            aspHeatView.highlightCellSelectionsByNames(selectionNames);

            //Highlight on Metadata
            var metaView = new metaDataHeatmapViewer();
            metaView.highlightCellSelectionsByNames(selectionNames);


	  }
	},
	{
	xtype: 'button',
	  text: 'Highlight with text',
	  handler: function() {
	    	var selectionTable = Ext.getCmp('cellSelectionTable');
    		var selectedItems = selectionTable.getSelectionModel().getSelected();

    		    var selectionNames = []
    		    for(var i = 0; i < selectedItems.length; i++){
              selectionNames.push(selectedItems.getAt(i).getData().selectionname);
    		    }

            // Highlight on heatmap
            var heatV = new heatmapViewer();
            heatV.highlightCellSelectionsByNames(selectionNames);
            pagHelpers.regC(72);

            // Highlight on embedding
            var embCntr = new embeddingViewer();
            embCntr.highlightSelectionsByNames(selectionNames, true);

            // Highlight on Aspects
            var aspHeatView = new aspectHeatmapViewer();
            aspHeatView.highlightCellSelectionsByNames(selectionNames);

            //Highlight on Metadata
            var metaView = new metaDataHeatmapViewer();
            metaView.highlightCellSelectionsByNames(selectionNames);

	  }
	},
	{
	  xtype: 'button',
	  text: 'Change Color',
	  handler:
	  function(){
	    var selectionTable = Ext.getCmp('cellSelectionTable');
		  var selectedItems = selectionTable.getSelectionModel().getSelected();
		  if (selectedItems.length === 1) {
		    var cellSelCntrl = new cellSelectionController();
		    var selectionName = selectedItems.getAt(0).getData().selectionname;
		    var oldColor = cellSelCntrl.getColor(selectionName);
        Ext.create('Ext.window.Window',{
	        title:'Change Cell Selection Color',
  	      id: 'cellSelectionColorWindow',
	        align:"center",
	        width: 300,
	        modal: true,
	        items:[
              {
                xtype:"colorfield",
                fieldLabel: 'Highlight Color',
                id: "colorPicker",
                labelWidth: 75,
                value: oldColor,
                listeners: {
                  change: 'onChange'
                }
            },
  	        {
  	          xtype: 'button',
	            text: 'Ok',
	            width:"20%",
	            height:"30%",
	            align: "center",
	            margin: "5 5 5 5",
	            handler: function(){

	              cellSelCntrl.setColor(selectedItems.getAt(0).getData().selectionname, "#" + (Ext.getCmp("colorPicker").value))
	              var heatView = new heatmapViewer();
                var aspHeatView = new aspectHeatmapViewer();
                var embCntr = new embeddingViewer();
                var metaHeatView = new metaDataHeatmapViewer();
                heatView.highlightCellSelectionByName(selectionName);
                aspHeatView.highlightCellSelectionByName(selectionName);
                metaHeatView.highlightCellSelectionByName(selectionName);
                embCntr.highlightSelectionByName(selectionName);

	              Ext.getCmp('cellSelectionColorWindow').close();
	            }
	          },
	          {
	            xtype: 'button',
	            text: 'Cancel',
  	          width:"20%",
	            height:"30%",
	            align: "center",
	            margin: "5 5 5 5",
	            handler: function(){
	              Ext.getCmp('cellSelectionColorWindow').close();
	            }
	          },
	        ]
  	    }).show();
		} else {
		    Ext.MessageBox.alert('Warning', 'Please choose only one cell selection first');
		}

	  }
	},
	{
	  xtype: 'button',
	  text: 'Regex Selection',
	  handler:
	  function(){

	    Ext.MessageBox.prompt("Regular Expression Selection", "Create Selection Using a Regular Expression",function(btn,text){
	      if(btn === "ok"){
	        var re = new RegExp(text);
	        var selection = [];
	        var dataCtrl = new dataController();
	        dataCtrl.getCellOrder(function(data){
	          for(var i = 0; i < data.length; i++){
	            if(data[i].match(re)){
	              selection.push(data[i])
	            }
	          }
	        })
	        if(selection.length > 0){

          	thisViewer.promptName(text.split(",").join(""), function(newDisplayName){
          	  var cellSelCntr =  new cellSelectionController();
          	  if(newDisplayName !== false){
          	    cellSelCntr.setSelection(newDisplayName ,selection);
          	  }
          	})
	        }
	      }
	    })
	  }
	}

    ]
    });

    uipanel.add(formPanel);

}

cellSelectionUIcontroller.prototype.promptName = function(curDisplay, callback){
    		    Ext.Msg.prompt('Rename Cell Selection', 'Please enter a new name:', function(btn, text) {
        			if (btn =='ok') {
      			    var newName = text;
      			    var cellSelCntr = new cellSelectionController();
      			    var cellSelUICntr = new cellSelectionUIcontroller();
        			 var re = new RegExp('[^A-Za-z0-9_]');
      			    if (newName.length === 0) {
      				    Ext.MessageBox.alert('Error','You must enter a selection name',function(e){
        			      cellSelUICntr.promptName(newName, callback);
        			    });
      			    }
      			    else if (newName.match(re) ) {
      				    Ext.MessageBox.alert('Error', 'The name must only contain letters, numbers and underscores (_)',function(e){
        			      cellSelUICntr.promptName(newName, callback);
        			    });
      			    }
      			    else if (cellSelCntr.displayNameExists(newName)) {
      				    Ext.MessageBox.alert('Error', 'A selection with this name already exists!',function(e){
        			      cellSelUICntr.promptName(newName, callback);
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

