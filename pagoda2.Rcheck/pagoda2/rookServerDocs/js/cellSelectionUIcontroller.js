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
    this.selectionFont = 12;
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
    var cellSelCntr = new cellSelectionController();
    var availSelections = cellSelCntr.getAvailableSelections();

    for (var sel in availSelections) {
        var selName = availSelections[sel];
        var selCount = cellSelCntr.getSelection(selName).length;
        var selDisplayName = cellSelCntr.getSelectionDisplayName(selName);
        var selColor = cellSelCntr.getColor(selName);

        store.add({
            selectionname: selName,
            displayname: selDisplayName,
            cellcount: selCount,
            color: selColor
        });
    } // for
}

/**
 * Generate the cell selection store
 */
cellSelectionUIcontroller.prototype.generateCellSelectionStore = function() {
    Ext.create('Ext.data.Store', {
        storeId: 'cellSelectionStoreForSelectionTable',
        fields: [{
                name: 'selectionname',
                type: 'string'
            },
            {
                name: 'displayname',
                type: 'string'
            },
            {
                name: 'cellcount',
                type: 'integer'
            }
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
    var toolbar = this.generateToolbar();
    var cellTableSelectionModel = Ext.create('Ext.selection.CheckboxModel', {});
    var cellSelectionTable = Ext.create('Ext.grid.Panel', {
        title: 'Cell Selections',
        id: 'cellSelectionTable',
        height: "100%",
        width: "100%",
        tools: [toolbar],
        scrollable: true,
        store: Ext.data.StoreManager.lookup('cellSelectionStoreForSelectionTable'),
        columns: [{
                text: 'Name',
                dataIndex: 'displayname',
                width: '62.7%'
            },
            {
                text: 'Count',
                dataIndex: 'cellcount',
                width: '28%'
            },
            {
                text: "&#x03DF;",
                dataIndex: 'color',
                width: '5%',
                renderer: function(value, meta) {
                    meta.style = "background-color:" + value + ";";
                },
            }
        ],
        emptyText: "No cell selections are currently available",
        singleSelect: false,
        selModel: cellTableSelectionModel,
        listeners: {

            //on response to double click color picker window is generated to change the color
            celldblclick: function(thisGrid, td, col, record, tr, row) {
                if (col === 3) {
                    var selectionName = Ext.getCmp('cellSelectionTable').getStore().getAt(row).getData().selectionname;
                    var cellSelCntrl = new cellSelectionController();
                    var oldColor = cellSelCntrl.getColor(selectionName);

                    //creates window with color picker to cahnge a color
                    Ext.create('Ext.window.Window', {
                        title: 'Change Cell Selection Color',
                        id: 'cellSelectionColorWindow',
                        align: "center",
                        width: 300,
                        modal: true,
                        items: [{
                                xtype: "colorfield",
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
                                width: "20%",
                                height: "30%",
                                align: "center",
                                margin: "5 5 5 5",
                                handler: function() {

                                    cellSelCntrl.setColor(selectionName, "#" + (Ext.getCmp("colorPicker").value))
                                    Ext.getCmp('cellSelectionColorWindow').close();
                                }
                            },
                            {
                                xtype: 'button',
                                text: 'Cancel',
                                width: "20%",
                                height: "30%",
                                align: "center",
                                margin: "5 5 5 5",
                                handler: function() {
                                    Ext.getCmp('cellSelectionColorWindow').close();
                                }
                            },
                        ]
                    }).show();

                }
            },
        }
    });

    uipanel.add(cellSelectionTable);
    this.generateToolbar();
}

/**
 * Builds the tool bar for the cell selection controller
 */
cellSelectionUIcontroller.prototype.generateToolbar = function() {

    var toolbar = Ext.create("Ext.Toolbar");
    var thisViewer = this;

    var highlightMenu = Ext.create("Ext.menu.Menu", {
        items: [{
                text: "Highlight",
                handler: function() {
                    var selectionTable = Ext.getCmp('cellSelectionTable');
                    var selectedItems = selectionTable.getSelectionModel().getSelected();

                    var selectionNames = []
                    for (var i = 0; i < selectedItems.length; i++) {
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
                text: "Highlight with Labels",
                handler: function() {
                    var selectionTable = Ext.getCmp('cellSelectionTable');
                    var selectedItems = selectionTable.getSelectionModel().getSelected();

                    var selectionNames = []
                    for (var i = 0; i < selectedItems.length; i++) {
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
                text: "Change Highlight",
                handler: function() {
                    var cellSelCntrl = new cellSelectionController();
                    var selectionTable = Ext.getCmp('cellSelectionTable');
                    var selectedItems = selectionTable.getSelectionModel().getSelected();
                    if (selectedItems.length === 0) {
                        Ext.MessageBox.alert('Warning', 'Please pick a cell selection first.');
                        return;
                    }
                    var oldColor = cellSelCntrl.getColor(selectedItems.getAt(0).getData().selectionname);
                    Ext.create('Ext.window.Window', {
                        title: 'Change Cell Selection Color',
                        id: 'cellSelectionColorWindow',
                        align: "center",
                        width: 300,
                        modal: true,
                        items: [{
                                xtype: "colorfield",
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
                                width: "20%",
                                height: "30%",
                                align: "center",
                                margin: "5 5 5 5",
                                handler: function() {

                                    var selectionNames = [];
                                    for (var i = 0; i < selectedItems.length; i++) {
                                        selectionNames.push(selectedItems.getAt(i).getData().selectionname);
                                    }

                                    for (var i = 0; i < selectionNames.length; i++) {
                                        cellSelCntrl.setColor(selectionNames[i], "#" + (Ext.getCmp("colorPicker").value), true)
                                    }
                                    cellSelCntrl.raiseSelectionChangedEvent();
                                    Ext.getCmp('cellSelectionColorWindow').close();

                                }
                            },
                            {
                                xtype: 'button',
                                text: 'Cancel',
                                width: "20%",
                                height: "30%",
                                align: "center",
                                margin: "5 5 5 5",
                                handler: function() {
                                    Ext.getCmp('cellSelectionColorWindow').close();
                                }
                            },
                        ]
                    }).show();
                }
            },
            {
                fieldLabel: 'Label size',
                xtype: 'numberfield',
                tooltip: 'Highlight label font size',
                value: thisViewer.selectionFont, // 2
                minValue: 4,
                maxValue: 50,
                disabled: false,
                listeners: {
                    change: {
                        fn: function(f, v) {
                            thisViewer.selectionFont = v;
                        }
                    }
                }
            }
        ]
    });

    toolbar.add({
        xtype: 'button',
        glyph: 0xf055, //fa-plus-circle
        tooltip: 'Merge',
        handler: function() {
            var selectionTable = Ext.getCmp('cellSelectionTable');
            var selectedItems = selectionTable.getSelectionModel().getSelected();
            if (selectedItems.length >= 2) {
                var selectionNames = [];
                for (var i = 0; i < selectedItems.length; i++) {
                    selectionNames.push(selectedItems.getAt(i).getData().selectionname);
                }

                thisViewer.promptName("", function(newDisplayName) {
                    var cellSelCntr = new cellSelectionController();
                    if (newDisplayName !== false) {
                        cellSelCntr.mergeSelectionsIntoNew(selectionNames, newDisplayName);
                    }
                })
            } else {
                Ext.MessageBox.alert('Warning', 'Please pick at least two cell selections to merge first.');
            }
        }

    });
    toolbar.add({
        xtype: "button",
        glyph: 0xf056, //fa-minus-circle
        tooltip: "Difference",
        handler: function() {
            var selectionTable = Ext.getCmp('cellSelectionTable');
            var selectedItems = selectionTable.getSelectionModel().getSelected();
            if (selectedItems.length >= 2) {
                var selectionNames = [];
                for (var i = 0; i < selectedItems.length; i++) {
                    selectionNames.push(selectedItems.getAt(i).getData().selectionname);
                }
                thisViewer.promptName("", function(newDisplayName) {
                    var cellSelCntr = new cellSelectionController();
                    if (newDisplayName !== false) {
                        cellSelCntr.differenceSelectionsIntoNew(selectionNames, newDisplayName);
                    }
                })

            } else {
                Ext.MessageBox.alert('Warning', 'Please pick at least two cell selections to find the difference of first.');
            }
        }
    });
    toolbar.add({
        xtype: 'button',
        glyph: 0xf042, //fa-adjust
        tooltip: 'Intersect',
        handler: function() {
            var selectionTable = Ext.getCmp('cellSelectionTable');
            var selectedItems = selectionTable.getSelectionModel().getSelected();
            if (selectedItems.length >= 2) {
                var selectionNames = [];
                for (var i = 0; i < selectedItems.length; i++) {
                    selectionNames.push(selectedItems.getAt(i).getData().selectionname);
                }
                thisViewer.promptName("", function(newDisplayName) {
                    var cellSelCntr = new cellSelectionController();
                    if (newDisplayName !== false) {
                        cellSelCntr.intersectSelectionsIntoNew(selectionNames, newDisplayName);
                    }
                })
            } else {
                Ext.MessageBox.alert('Warning', 'Please pick at least two cell selections to intersect first.');
            }
        }
    });
    toolbar.add({
        xtype: "button",
        glyph: 0xf057, //fa-stop-circle
        tooltip: "Compliment",
        handler: function() {
            var selectionTable = Ext.getCmp('cellSelectionTable');
            var selectedItems = selectionTable.getSelectionModel().getSelected();
            if (selectedItems.length >= 1) {
                var selectionNames = [];
                for (var i = 0; i < selectedItems.length; i++) {
                    selectionNames.push(selectedItems.getAt(i).getData().selectionname);
                }
                thisViewer.promptName("", function(newDisplayName) {
                    var cellSelCntr = new cellSelectionController();
                    if (newDisplayName !== false) {
                        cellSelCntr.complimentSelectionsIntoNew(selectionNames, newDisplayName);
                    }
                })


            } else {
                Ext.MessageBox.alert('Warning', 'Please pick at least one cell selection to compliment first.');
            }
        }
    });
    toolbar.add({
        xtype: 'tbseparator'
    });
    toolbar.add({
        xtype: 'button',
        glyph: 0xf24d, //fa-clone
        tooltip: 'Copy Selection',
        handler: function() {
            var selectionTable = Ext.getCmp('cellSelectionTable');
            var selectedItems = selectionTable.getSelectionModel().getSelected();
            if (selectedItems.length === 1) {
                var oldSelectionName = selectedItems.getAt(0).getData().selectionname;
                var oldDisplayName = selectedItems.getAt(0).getData().displayname;

                thisViewer.promptName(oldDisplayName, function(newDisplayName) {
                    var cellSelCntr = new cellSelectionController();
                    if (newDisplayName !== false) {
                        cellSelCntr.duplicateSelection(oldSelectionName, newDisplayName);
                    }
                })


            } else {
                Ext.MessageBox.alert('Warning', 'Please choose only one cell selection first');
            }

        }
    });
    toolbar.add({
        xtype: 'button',
        glyph: 0xf246, //fa-I-cursor
        tooltip: 'Rename',
        handler: function() {
            var selectionTable = Ext.getCmp('cellSelectionTable');
            var selectedItems = selectionTable.getSelectionModel().getSelected();
            if (selectedItems.length === 1) {
                var oldDisplayName = selectedItems.getAt(0).getData().displayname;
                var oldSelectionName = selectedItems.getAt(0).getData().selectionname;
                thisViewer.promptName(oldDisplayName, function(newDisplayName) {
                    var cellSelCntr = new cellSelectionController();
                    if (newDisplayName !== false) {
                        cellSelCntr.renameSelection(oldSelectionName, newDisplayName);
                    }
                })
            } else {
                Ext.MessageBox.alert('Warning', 'Please choose only one cell selection first');
            }
        }
    });
    toolbar.add({
        xtype: 'button',
        glyph: 0xf1f8, // fa-trash
        tooltip: 'Delete',
        handler: function() {
            var selectionTable = Ext.getCmp('cellSelectionTable');
            var selectedItems = selectionTable.getSelectionModel().getSelected();
            if (selectedItems.length >= 1) {
                var dispNames = [];
                var selNames = [];
                for (var i = 0; i < selectedItems.length; i++) {
                    selNames.push(selectedItems.getAt(i).getData().selectionname);
                    dispNames.push(selectedItems.getAt(i).getData().displayname)
                }
                Ext.Msg.show({
                    title: 'Delete Selection',
                    msg: "Would you like to delete the following selections? <br>" + dispNames.join("<br>"),
                    buttons: Ext.Msg.OKCANCEL,
                    fn: function(btn, text) {
                        if (btn === 'ok') {
                            var cellSel = new cellSelectionController();
                            selNames.forEach(function(selName) {
                                cellSel.deleteSelection(selName);
                            });
                        }
                    }
                });
            } else {
                Ext.MessageBox.alert('Warning', 'Please select a cell selection first');
            }
        }
    });
    toolbar.add({
        xtype: 'button',
        tooltip: 'Highlight Options',
        glyph: 0xf1fc, //fa-paint-brush
        menu: highlightMenu
    });
    toolbar.add({
        xtype: 'tbseparator'
    });
    toolbar.add({
        xtype: 'button',
        glyph: 0xf002, //fa-search
        tooltip: 'Regex Selection',
        handler: function() {

            Ext.MessageBox.prompt("Regular Expression Selection", "Create Selection Using a Regular Expression", function(btn, text) {
                if (btn === "ok") {
                    var re = new RegExp(text);
                    var selection = [];
                    var dataCtrl = new dataController();
                    dataCtrl.getCellOrder(function(data) {
                        for (var i = 0; i < data.length; i++) {
                            if (data[i].match(re)) {
                                selection.push(data[i])
                            }
                        }
                    })
                    if (selection.length > 0) {
                        thisViewer.promptName(text.split(",").join(""), function(newDisplayName) {
                            var cellSelCntr = new cellSelectionController();
                            if (newDisplayName !== false) {
                                cellSelCntr.setSelection(selection, newDisplayName);
                            }
                        })
                    }
                }
            })
        }
    });
    toolbar.add({
        xtype: 'tbseparator'
    });
    toolbar.add({
        xtype: 'button',
        glyph: 0xf0c7, //fa-floppy-o
        tooltip: 'Export Selected',
        handler: function() {
            var selectionTable = Ext.getCmp('cellSelectionTable');
            var selectedItems = selectionTable.getSelectionModel().getSelected();
            if (selectedItems.length >= 1) {

                var selectionFormatted = [];
                var cellSelCntr = new cellSelectionController();
                for (var index = 0; index < selectedItems.length; index++) {
                    var selectionName = selectedItems.getAt(index).getData().selectionname;
                    var displayName = selectedItems.getAt(index).getData().displayname;
                    var color = selectedItems.getAt(index).getData().color.substring(1);
                    var selection = cellSelCntr.getSelection(selectionName);
                    selectionFormatted.push(color + "," + displayName + "," + selection.join(","));
                }
                window.open('data:application/csv;charset=utf-8,' + encodeURI(selectionFormatted.join("\n")));
            } else {
                Ext.MessageBox.alert('Warning', 'Please choose one or more cell selections first');
            }
        }

    });
    toolbar.add({
        xtype: 'button',
        glyph: 0xf115, //fa-folder-open-o
        tooltip: 'Import Selections',
        handler: function() {
            // Define a store for the options

            if (!Ext.getCmp('cellFileSelectionWindow')) {
                Ext.create('Ext.window.Window', {
                    title: 'Cell File Selection',
                    id: 'cellFileSelectionWindow',
                    height: 100,
                    width: 500,
                    align: "center",
                    modal: true,
                    resizeable: false,
                    items: [{
                            height: "12px",
                            html: '<input type="file" id="selectedCellFile"><br>'
                        },
                        Ext.create('Ext.panel.Panel', {
                            layout: 'hbox',
                            items: [{
                                    xtype: 'button',
                                    text: 'Ok',
                                    width: "45px",
                                    height: "25px",
                                    columnWidth: "20%",
                                    margin: "5 5 5 5",
                                    align: "center",
                                    handler: function() {

                                        // Import the selected file

                                        var dataCntr = new dataController();
                                        var cellSelFile = document.getElementById("selectedCellFile").files[0];
                                        var cellSelFileName = cellSelFile.name;
                                        var reader = new FileReader();

                                        reader.onload = function(progressEvent) {
                                            var reader = this;

                                            dataCntr.getCellOrder(function(cellOrder) {

                                                var params = {};
                                                params.total = 0;
                                                params.lines = reader.result.split("\n");
                                                params.removedCells = {};
                                                params.cellOrder = cellOrder;

                                                for (var i = params.lines.length -1; i > 0; i--) {
                                                  if (params.lines[i].length === 0) {
                                                    params.lines.splice(i, 1)
                                                  }
                                                }

                                                var functionStep = function(params, i, step, max) {
                                                    var cellSelCntrl = new cellSelectionController();
                                                    for (var line = 0; line < step; line++) {
                                                        var selection = params.lines[line + i].split(",");
                                                        var color = "#" + selection.shift();
                                                        var dispName = selection.shift();
                                                        params.removedCells[dispName] = 0;
                                                        var pureSelection = [];
                                                        for (var elem = 0; elem < selection.length; elem++) {
                                                            if (params.cellOrder.includes(selection[elem])) {
                                                                pureSelection.push(selection[elem]);
                                                            } else {
                                                                params.removedCells[dispName]++;
                                                            }
                                                        }
                                                        if (params.removedCells[dispName] !== selection.length) {
                                                            while (cellSelCntrl.displayNameExists(dispName)) {
                                                                dispName = dispName + "~RecentlyLoaded"
                                                            }
                                                            cellSelCntrl.setSelection(pureSelection, dispName, {}, color, undefined, true);
                                                            params.total++;
                                                        }
                                                    }
                                                }

                                                var callback = function(params) {
                                                    var extraInfo = "";
                                                    for (var selName in params.removedCells) {
                                                        if (params.removedCells[selName] > 0) {
                                                            extraInfo += "<br>" + params.removedCells[selName] + " cell(s) could not be loaded from selection " + selName;
                                                        }
                                                    }
                                                    Ext.MessageBox.alert('Load Cell Selections Complete', params.total + " selections were generated from the data within " + cellSelFileName + extraInfo)
                                                    var cellSel = new cellSelectionController();
                                                    cellSel.raiseSelectionChangedEvent();
                                                }

                                                pagHelpers.generateProgressBar(functionStep, params.lines.length, 1, callback, params);

                                            });
                                        };

                                        reader.readAsText(cellSelFile);
                                        Ext.getCmp('cellFileSelectionWindow').close();
                                    }
                                },
                                {
                                    xtype: 'button',
                                    text: 'Cancel',
                                    width: "70px",
                                    height: "25px",
                                    columnWidth: "20%",
                                    margin: "5 5 5 5",
                                    align: "center",
                                    handler: function() {
                                        Ext.getCmp('cellFileSelectionWindow').close();
                                    }
                                }
                            ]
                        })
                    ],
                }).show();
            }
            Ext.getCmp('cellFileSelectionWindow').focus();

        }
    });

    return toolbar;
}

/**
 * Prompts the user for a name and doesn't let them leave the prompt until cancelled request or provide a name that is passable
 * @param {curDisplay} what, if any, text will appear in the box
 * @param {callback}
 */
cellSelectionUIcontroller.prototype.promptName = function(curDisplay, callback) {
    Ext.Msg.prompt('Rename Cell Selection', 'Please enter a new name:', function(btn, text) {
        if (btn == 'ok') {
            var newName = text;
            var cellSelCntr = new cellSelectionController();
            var cellSelUICntr = new cellSelectionUIcontroller();
            var re = new RegExp('[\,]');
            if (newName.length === 0) {
                Ext.MessageBox.alert('Error', 'You must enter a selection name', function(e) {
                    cellSelUICntr.promptName(newName, callback);
                });
            } else if (newName.match(re)) {
                Ext.MessageBox.alert('Error', 'The name must not contain a comma', function(e) {
                    cellSelUICntr.promptName(newName, callback);
                });
            } else if (cellSelCntr.displayNameExists(newName)) {
                Ext.MessageBox.alert('Error', 'A selection with this name already exists', function(e) {
                    cellSelUICntr.promptName(newName, callback);
                });
            } else {
                callback(newName)
            }
        } else {
            callback(false);
        }
    }, {}, false, curDisplay);
}
