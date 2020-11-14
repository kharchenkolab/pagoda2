"use strict";

/*
 * Filename: metaDataHeatmapViewer.js
 * Author: Nikolas Barkas
 * Data: March 2017
 */

/**
 * Manages the metadata heatmap viewer
 * @constructor
 */
function metaDataHeatmapViewer() {
    if (typeof metaDataHeatmapViewer.instance === 'object') {
        return metaDataHeatmapViewer.instance;
    }

    // The initializer is called externally when the
    // dendrogram (which will provide the order)
    // is ready

    var extJsContainer = Ext.getCmp('metadataPanel');
    extJsContainer.onResize = function() {
        var metaView = new metaDataHeatmapViewer();
        metaView.updateCanvasSize();
        metaView.drawMetadata();
    };

    // Keep track of what selection we are showing so
    // we can persist accross redraws
    this.currentOverlaySelectionName = null;
    this.currentOverlaySelectionNames = null;
    this.currentOverlaySelectionShown = false;


    metaDataHeatmapViewer.instance = this;
}

/**
 * Get the height
 */
metaDataHeatmapViewer.prototype.getHeight = function() {
    return Ext.getCmp('metadataPanel').getHeight() - 5;
}

/**
 * Get the width
 */
metaDataHeatmapViewer.prototype.getWidth = function() {
    return (Ext.getCmp('metadataPanel').getWidth());
}


/**
 * Initialise the metadata heatmap viewer
 * @description this is separate from the constructor
 * because it's called by the container object when things
 * are ready
 */
metaDataHeatmapViewer.prototype.initialize = function() {

    var metadataContainer = $('#metadata-area-container');
    var contextMenuOpen = false;
    metadataContainer.append('<div id="metadata-area-container-inner"></div>');

    // We need an inner container for establishing a new
    // reference for the absolute positioning of the two canvases
    var metadataContainerInner = $('#metadata-area-container-inner');
    metadataContainerInner.css({
        position: 'relative'
    });

    metadataContainerInner.append(
        '<canvas id="metadata-area"></canvas>' +
        '<canvas id="metadata-area-selection"></canvas>' +
        '<canvas id="metadata-area-overlay"></canvas>'
    );

    var metadataArea = $('#metadata-area');
    metadataArea.css({
        position: 'absolute',
        top: 0,
        left: 0
    });

    var metadataAreaSelection = $('#metadata-area-selection');
    metadataAreaSelection.css({
        position: 'absolute',
        top: 0,
        left: 0
    });

    var metadataAreaOverlay = $('#metadata-area-overlay');
    metadataAreaOverlay.css({
        position: 'absolute',
        top: 0,
        left: 0
    });

    // Clickable regions that are used for identifying the row
    this.clickRegionsRows = new clickableRegions();

    // clickableRegions that are used to identify each individual entry
    this.clickRegionsEntries = new clickableRegions();

    // Setup the click listeners
    // NOTE: At this point we are only interested in resoving
    // which line we are clicking on. In the future some of the
    // metadata should support clicking on individual cells and/or
    // regions of contigious properties.
    // For clicking on contigious chucks (e.g. clusters) we
    // will need a propery that says the the data is guaranteed to be
    // contigious with the default ordering
    // State for drag select
    this.primaryMouseButtonDown = false;
    this.dragging = false;
    this.dragStartX = null;


    (metadataAreaOverlay[0]).addEventListener('mousedown', function(e) {
        // For preventing selection on double click
        e.preventDefault();
        var heatView = new metaDataHeatmapViewer();
        var drawConsts = heatView.getDrawConstants();
        if (!contextMenuOpen & e.offsetX > drawConsts.left & e.offsetX < drawConsts.left + drawConsts.width & e.which == 1) {
            heatView.primaryMouseButtonDown = true;
            heatView.dragStartX = e.offsetX;
        }
    });

    (metadataAreaOverlay[0]).addEventListener('mouseup', function(e) {
        var heatDendView = new heatmapDendrogramViewer();
        var metaView = new metaDataHeatmapViewer();
        if (metaView.primaryMouseButtonDown) {
            metaView.primaryMouseButtonDown = false;

            if (metaView.dragging) {
                // End of drag
                metaView.dragging = false;

                // Range of X is metaView.dragStartX  to e.offsetX

                var drawConsts = metaView.getDrawConstants();

                var dendV = new dendrogramViewer();
                var curDisplayIdxs = dendV.getCurrentDisplayCellsIndexes();
                var metaWidth = drawConsts.width - heatDendView.getPlotAreaRightPadding();


                // Start and end as percent of current display cell range
                var startPC = (metaView.dragStartX - drawConsts.left) / metaWidth;
                var endPC = (e.offsetX - drawConsts.left) / metaWidth;

                // For left to right drag
                if (startPC > endPC) {
                    var tmp = startPC;
                    startPC = endPC;
                    endPC = tmp;
                };

                // Avoid out of bounds issues
                if (endPC > 1) {
                    endPC = 1
                };
                if (startPC < 0) {
                    startPC = 0
                };

                var ncells = curDisplayIdxs[1] - curDisplayIdxs[0];

                var startIndex = Math.floor(startPC * ncells)
                var endIndex = Math.floor(endPC * ncells);

                var cellsForSelection = dendV.getCurrentDisplayCells().slice(startIndex, endIndex);

                var cellSelCntr = new cellSelectionController();
                var selectionName = cellSelCntr.setSelection(cellsForSelection, 'Heatmap Selection', new Object(), "#FF0000", 'heatmapSelection');

                // Highlight on heatmap
                var metaView = new heatmapViewer();
                metaView.highlightCellSelectionByName(selectionName);

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
                var x = e.offsetX;
                var y = e.offsetY;

                var mdhv = new metaDataHeatmapViewer();
                mdhv.clickRegionsRows.resolveClick(x, y, function(params) {
                    var embV = new embeddingViewer();
                    embV.setColorConfiguration('metadata');
                    // Here  we are just passing the params from the
                    // click region registration, we might want to change this
                    // later, but in any case keep processin in here to the minimum
                    embV.setMetadataColorInfo(params);
                    embV.updateColors();
                });
            }
        }
    });

    (metadataAreaOverlay[0]).addEventListener('contextmenu', function(e) {
        e.preventDefault();
        var x = e.offsetX;
        var y = e.offsetY;
        contextMenuOpen = true;
        var mdhv = new metaDataHeatmapViewer();
        mdhv.clickRegionsEntries.resolveClick(x, y, function(params) {
            // params.cellid
            // params.keyLabel params.valueLabel
            var defaultLabel = params.keyLabel + '_' + params.valueLabel;
            defaultLabel = defaultLabel.split(/\ |\#/).join("");
            var contextMenu = new Ext.menu.Menu({
                items: [{
                        text: "Identify Cell",
                        handler: function() {
                            var mdhv = new metaDataHeatmapViewer();
                            mdhv.clickRegionsEntries.resolveClick(x, y, function(params) {
                                var stsBar = new statusBar();
                                var msg = 'Cell: ' + params.cellid + ' ' + '(' + params.keyLabel + ': ' + params.valueLabel + ')';
                                stsBar.showMessage(msg);

                                var embViewer = new embeddingViewer();
                                embViewer.highlightCellByID(params.cellid);
                            });
                        }
                    },
                    {
                        text: 'Generate selection from cluster',
                        handler: function() {
                            Ext.MessageBox.prompt('New name', 'Name for new selection:',
                                function(btn, text) {
                                    // Make new selection
                                    if (btn === 'ok') {
                                        var cellSelCntr = new cellSelectionController();

                                        var newSelectionDisplayName = text;

                                        var re = new RegExp('[,]');
                                        if (newSelectionDisplayName.length === 0) {
                                            Ext.MessageBox.alert('Error', 'You must enter a selection name');
                                        } else if (newSelectionDisplayName.match(re)) {
                                            Ext.MessageBox.alert('Error',
                                                'The name must not contain a comma');
                                        } else {
                                            if (cellSelCntr.displayNameExists(newSelectionDisplayName)) {
                                                Ext.MessageBox.alert(
                                                    'Error',
                                                    'A selection with this name already exists!');
                                            } else {
                                                // Make the slection here
                                                var key = params.key;
                                                var value = params.value;
                                                mdhv.makeCellSelectionFromMetadata(key, value, newSelectionDisplayName, true, true);
                                            }
                                        } // if lenth == 0
                                    } // if btn == ok
                                }, this, false, defaultLabel); // Message box prompthandler
                        },
                    }, //Item 1
                    {
                        text: "Generate selection for each cluster",
                        handler: function() {
                            Ext.MessageBox.prompt("Set Limit", "Specify a lower limit for number of cells in a new selection.",
                                function(btn, text) {
                                    var rejectionFunction = function(selection) {
                                        return true;
                                    };

                                    if (btn === "ok") {
                                        if (!isNaN(text)) {
                                            var lowerLimit = parseInt(text);
                                            rejectionFunction = function(selection) {
                                                return selection.length >= lowerLimit;
                                            }
                                        }
                                        mdhv.makeAllCellSelectionsFromMetadataProgress(params.key, (params.keyLabel + "_").split(/\ |\#/).join(""), rejectionFunction)
                                    }
                                }, this, false, "50")

                        }
                    },
                    {
                        text: "Show Legend",
                        handler: function() {
                            mdhv.generateLegend(params.key)
                        }
                    }

                ], //items
                listeners: {
                    'deactivate': function() {
                        contextMenuOpen = false;
                    },
                }
            }); //context menu

            contextMenu.showAt(e.clientX, e.clientY);

            return false;
        }); // resolve click

    }); // contextmenu event listener

    (metadataAreaOverlay[0]).addEventListener('mousemove', function(e) {
        var metaV = new metaDataHeatmapViewer();
        var heatV = new heatmapViewer();
        var aspeV = new aspectHeatmapViewer();
        var heatDendView = new heatmapDendrogramViewer();
        var dendV = new dendrogramViewer();
        var embV = new embeddingViewer();

        var x = e.offsetX;

        // Calculate cell index for embedding hover
        var drawConsts = metaV.getDrawConstants();
        var metaWidth = drawConsts.width - heatDendView.getPlotAreaRightPadding();
        var posPC = (x - drawConsts.left) / metaWidth;

        if (posPC > 0 && posPC < 1) {
          var curDisplayIdxs = dendV.getCurrentDisplayCellsIndexes();
          var cellindex = Math.floor(posPC * (curDisplayIdxs[1] - curDisplayIdxs[0])) + curDisplayIdxs[0];
          embV.highlightCellByIndex(cellindex);
        } else {
            embV.clearHighlightCell();
        }

        metaV.showOverlay(x);
        heatV.showOverlay(x);
        aspeV.showOverlay(x);

        if (metaV.primaryMouseButtonDown) {
            if (!metaV.dragging) {
                // The first mouse move after the mouse down
                // Initiate dragging process
                // This is for resetting the current selection params not for the actual clear
                metaV.clearSelectionOverlay();
                metaV.dragging = true;
            }

            // Clear the canvas
            var canvas = document.getElementById('metadata-area-selection');
            var ctx = canvas.getContext('2d');
            var width = canvas.width;
            var height = canvas.height;
            ctx.clearRect(0, 0, width, height);

            var drawConsts = metaV.getDrawConstants();
            var actualPlotHeight = drawConsts.height;

            var heatDendView = new heatmapDendrogramViewer();

            var boundedX;
            if (x < drawConsts.left) {
                boundedX = drawConsts.left;
            } else if (x > drawConsts.left + drawConsts.width - heatDendView.getPlotAreaRightPadding()) {
                boundedX = drawConsts.left + drawConsts.width - heatDendView.getPlotAreaRightPadding();
            } else {
                boundedX = x;
            }

            ctx.save();
            ctx.beginPath();
            ctx.fillStyle = 'rgba(255,0,0,0.5)';
            ctx.fillRect(metaV.dragStartX, drawConsts.top, boundedX - metaV.dragStartX, actualPlotHeight);
            ctx.restore();
        } //if (metaV.primaryMouseButtonDown)

    }); // mousemove addEventListener

    (metadataAreaOverlay[0]).addEventListener('mouseout', function(e) {
        var metaV = new metaDataHeatmapViewer();
        var heatV = new heatmapViewer();

        metaV.clearOverlay();
        heatV.clearOverlay();

        var aspeV = new aspectHeatmapViewer();
        aspeV.clearOverlay();

        // Clear embedding hover
        var embV = new embeddingViewer();
        embV.clearHighlightCell();
    });

    // Pointer change to cross hairs when over the heatmap
    (metadataAreaOverlay[0]).addEventListener('mouseenter', function(e) {
        document.body.style.cursor = "crosshair";
    });

    (metadataAreaOverlay[0]).addEventListener('mouseout', function(e) {
        document.body.style.cursor = "default";
    });

    this.updateCanvasSize();
    
    // Do the intial draw and notify LoadingProgressTracker
    this.drawMetadata(function(){
      var eB = new eventBus();
      eB.publish('initialMetadataLoadComplete');
    });
}

/**
 * Show an overlay vertical line
 * @param x the x-coordinate to show the line at
 */
metaDataHeatmapViewer.prototype.showOverlay = function(x) {
    var overlayArea = document.getElementById('metadata-area-overlay');
    var ctx = overlayArea.getContext('2d');

    var heatDendView = new heatmapDendrogramViewer();
    var metaV = new metaDataHeatmapViewer();
    var drawConsts = metaV.getDrawConstants();

    ctx.clearRect(0, 0, overlayArea.width, overlayArea.height);
    ctx.setLineDash([10, 10]);
    ctx.lineWidth = 1;

    // Within the heatmap area
    if (x > drawConsts.left & x < drawConsts.width + drawConsts.left - heatDendView.getPlotAreaRightPadding()) {
        ctx.beginPath();
        ctx.moveTo(x, drawConsts.top);
        ctx.lineTo(x, drawConsts.height + drawConsts.top);
        ctx.stroke();
    }
}

/**
 * Clear the existing overlay
 */
metaDataHeatmapViewer.prototype.clearOverlay = function() {
    var overlayArea = document.getElementById('metadata-area-overlay');
    var ctx = overlayArea.getContext('2d');

    ctx.clearRect(0, 0, overlayArea.width, overlayArea.height);
}

/**
 * Clears the selection overlay
 */
metaDataHeatmapViewer.prototype.clearSelectionOverlay = function() {
    this.clearSelectionOverlayInternal();
    this.currentOverlaySelectionShown = false;
}

metaDataHeatmapViewer.prototype.clearSelectionOverlayInternal = function() {
    var canvas = document.getElementById('metadata-area-selection');
    var ctx = canvas.getContext('2d');
    var width = canvas.width;
    var height = canvas.height;
    ctx.clearRect(0, 0, width, height);
}

/**
 * Update the canvas size of this element with the
 * sized provided by thhe heatmapDendrogramViewer
 */
metaDataHeatmapViewer.prototype.updateCanvasSize = function() {
    var metaArea = $('#metadata-area')[0];
    var metadataAreaOverlay = $('#metadata-area-overlay')[0];

    var metadataContainer = $('#metadata-area-container');

    var heatDendV = new heatmapDendrogramViewer();

    var curWidth = this.getWidth();
    var curHeight = this.getHeight();

    this.canvasElementWidth = curWidth;
    this.canvasElementHeight = curHeight;

    metaArea.width = curWidth;
    metaArea.height = curHeight;

    metadataAreaOverlay.width = curWidth;
    metadataAreaOverlay.height = curHeight;


    var metadataAreaSelection = $('#metadata-area-selection')[0];
    metadataAreaSelection.width = curWidth;
    metadataAreaSelection.height = curHeight;


    metadataContainer.css({
        width: curWidth + 'px',
        height: curHeight + 'px'
    });
}

/**
 * Get the draw constants for the metadata heatmap
 */
metaDataHeatmapViewer.prototype.getDrawConstants = function() {
    var heatDendView = new heatmapDendrogramViewer();
    var dendV = new dendrogramViewer();

    return {
        top: 1,
        left: heatDendView.getPlotAreaLeftPadding(),
        width: this.getWidth(),
        height: this.getHeight(),

    };
}

/**
 * Draw an updated metadata heatmap
 *
 */
metaDataHeatmapViewer.prototype.drawMetadata = function(callback) {
    // Get cells currently displayed in the dendrogram
    var dendV = new dendrogramViewer();
    var cellSelection = dendV.getCurrentDisplayCells();

    // Some parameters to control the  drawing of the heatmap
    var plotConsts = this.getDrawConstants();
    var bottomPadding = 2;
    var heatDendView = new heatmapDendrogramViewer();
    var top = plotConsts.top;
    var left = plotConsts.left;
    var metaWidth = plotConsts.width - heatDendView.getPlotAreaRightPadding();
    var metaHeight = plotConsts.height - bottomPadding;
    // Request the data
    var dataCntr = new dataController();
    dataCntr.getCellMetadata(function(data) {

        // Get the canvas
        var canvas = $('#metadata-area')[0];
        var ctx = canvas.getContext("2d");

        // Clear the canvas
        // FIXME: Clear the proper area
        ctx.clearRect(0, 0, 5000, 5000);

        // Get number of cells and calculate cell size
        var nCells = cellSelection.length;
        var nrows = Object.keys(data).length;

        // Calculate cell size
        var cellWidth = metaWidth / (nCells);
        var cellHeight = metaHeight / nrows;

        // Calculate label position
        var labelYpad = cellHeight / 2 + 5;
        var labelXpad = 10;

        // Get  and clear the click areas
        var mdhv = new metaDataHeatmapViewer();
        mdhv.clickRegionsRows.clearClickAreas();
        mdhv.clickRegionsEntries.clearClickAreas()

        // For each metadata row
        var j = 0; // row counter
        for (var key in data) {
            // Skip if object property
            if (!data.hasOwnProperty(key)) continue;

            // Current plotting information
            var curLabel = data[key].displayname;
            if (typeof curLabel === 'undefined') {
                curLabel = ''
            }; // default to empty

            var curData = data[key].data;
            var curPal = data[key].palette;
            var curLevels = data[key].levels;
            if (typeof curLevels === 'undefined') {
                curLevels = curPal
            }; //Use colors if levels unavailable

            // Get the y coordinate
            var y = j * cellHeight + top;

            // Plot the row in the order provided by the dendrogram
            for (var i = 0; i < cellSelection.length; i++) {
                var curCell = cellSelection[i];

                // value to plot
                var val = curData[curCell]; // value
                var col = curPal[val];

                if (typeof col == 'undefined') {
                    console.warn('Level without a corresponding color found');
                    col = '#FFFFFFFF'; // Leave a white gap
                }

                var x = i * cellWidth + left;
                ctx.fillStyle = col.substr(0, 7); // color w/o alpha
                ctx.fillRect(x, y, cellWidth, cellHeight);

                // Register a click region for the particular entry
                mdhv.clickRegionsEntries.addClickArea(
                    x, y,
                    x, y + cellHeight,
                    x + cellWidth, y + cellHeight,
                    x + cellWidth, y, {
                        'key': key,
                        'keyLabel': curLabel,
                        cellid: curCell,
                        value: val,
                        valueLabel: curLevels[val],
                        totalLevels: curPal.length
                    }
                );
            }

            // Plot the label
            // Cap at 16 and don't plot if smaller than 6
            var fontSize = Math.min(cellHeight * 0.8, 14);
            if (fontSize >= 8) {
                ctx.font = fontSize + 'px Arial';
                ctx.fillStyle = 'black';
                var labelx = cellSelection.length * cellWidth + left + labelXpad;
                ctx.fillText(curLabel, labelx, y + labelYpad);
            }

            // Register a click region for this metadata row
            var y1 = j * cellHeight + top;
            var y2 = (j + 1) * cellHeight + top;

            var x1 = left;
            var x2 = left + metaWidth;

            mdhv.clickRegionsRows.addClickArea(
                x1, y1,
                x1, y2,
                x2, y2,
                x2, y1, {
                    metadataName: key
                }
            );

            // Increment row counter
            j++
        } // dataCntr.getCellMetadata

        // Plot a bounding box
        ctx.beginPath();
        ctx.rect(left, top, metaWidth, metaHeight);
        ctx.stroke();


        mdhv.clearSelectionOverlayInternal();
        if (mdhv.currentOverlaySelectionShown === true) {
            if (mdhv.currentOverlaySelectionName !== null) {
                mdhv.highlightCellSelectionByName(mdhv.currentOverlaySelectionName);
            } else {
                mdhv.highlightCellSelectionsByNames(mdhv.currentOverlaySelectionNames);
            }
        }

        // If a callback has been provided call it
        if(typeof callback == "function") {
          callback();
        }

    });
}

/**
 * get 2d drawing context for selection
 */
metaDataHeatmapViewer.prototype.getSelectionDrawingContext = function() {
    var canvas = document.getElementById('metadata-area-selection');
    var ctx = canvas.getContext('2d');
    return ctx;
}

/**
 * Highlight a cell selection given it's name
 */
metaDataHeatmapViewer.prototype.highlightCellSelectionByName = function(selectionName) {
    var metadataHeatV = this;

    this.currentOverlaySelectionName = selectionName;
    this.currentOverlaySelectionNames = null;
    this.currentOverlaySelectionShown = true;

    var dendV = new dendrogramViewer();

    // Get the cells in the cell selection to highlight
    var cellSelCntr = new cellSelectionController();
    var cellSelection = cellSelCntr.getSelection(selectionName);


    // Get the cell order
    var dataCntr = new dataController();
    dataCntr.getCellOrderHash(function(cellorderHash) {
        // Currently displayed cells
        var cellRange = dendV.getCurrentDisplayCellsIndexes();
        var ncells = cellRange[1] - cellRange[0];
        var ctx = metadataHeatV.getSelectionDrawingContext();
        ctx.clearRect(0, 0, 3000, 3000);

        var heatDendView = new heatmapDendrogramViewer();
        // Get and calculate plotting values
        var drawConsts = metadataHeatV.getDrawConstants();
        var heatmapWidth = drawConsts.width - heatDendView.getPlotAreaRightPadding();
        var cellWidth = heatmapWidth / ncells;
        var left = drawConsts.left;
        var n = cellSelection.length;

        var actualPlotHeight = drawConsts.height;

        ctx.save();

        // We need to split the color to components to add the alpha (Chrome issue)
        var selColor = pagHelpers.hexToRgb(cellSelCntr.getColor(selectionName));
        ctx.strokeStyle = 'rgba(' + selColor.r + ',' + selColor.g + ',' + selColor.b + ',0.2)';

        // Draw vertical lines for selected cells
        for (var i = 0; i < n; i++) {
            var cellIndex = cellorderHash[cellSelection[i]];

            // Cell is among currently displayed ones
            if (cellIndex < cellRange[1] && cellIndex > cellRange[0]) {
                var colIndex = cellIndex - cellRange[0];

                var x = colIndex * cellWidth + left;

                ctx.beginPath();
                ctx.moveTo(x, drawConsts.top);
                ctx.lineTo(x, actualPlotHeight);
                ctx.stroke();
            } // if
        } // for

        ctx.restore();

    }); // get the cell order
}

/**
 * Highlight a group of cell selections given a list of names
 */
metaDataHeatmapViewer.prototype.highlightCellSelectionsByNames = function(selectionNames) {
    var metadataHeatV = this;

    this.currentOverlaySelectionNames = selectionNames;
    this.currentOverlaySelectionName = null;
    this.currentOverlaySelectionShown = true;

    var dendV = new dendrogramViewer();
    var ctx = metadataHeatV.getSelectionDrawingContext();
    ctx.clearRect(0, 0, 3000, 3000);

    // Get the cells in the cell selection to highlight
    var cellSelCntr = new cellSelectionController();

    // Get the cell order
    var dataCntr = new dataController();
    dataCntr.getCellOrderHash(function(cellorderHash) {
        // Currently displayed cells
        selectionNames.forEach(function(selectionName) {
            var cellSelection = cellSelCntr.getSelection(selectionName);
            var cellRange = dendV.getCurrentDisplayCellsIndexes();
            var ncells = cellRange[1] - cellRange[0];

            var heatDendView = new heatmapDendrogramViewer();
            // Get and calculate plotting values
            var drawConsts = metadataHeatV.getDrawConstants();
            var heatmapWidth = drawConsts.width - heatDendView.getPlotAreaRightPadding();
            var cellWidth = heatmapWidth / ncells;
            var left = drawConsts.left;
            var n = cellSelection.length;

            var actualPlotHeight = drawConsts.height;

            ctx.save();
            var selColor = pagHelpers.hexToRgb(cellSelCntr.getColor(selectionName));
            ctx.strokeStyle = 'rgba(' + selColor.r + ',' + selColor.g + ',' + selColor.b + ',0.2)';

            // Draw vertical lines for selected cells
            for (var i = 0; i < n; i++) {
                var cellIndex = cellorderHash[cellSelection[i]];

                // Cell is among currently displayed ones
                if (cellIndex < cellRange[1] && cellIndex > cellRange[0]) {
                    var colIndex = cellIndex - cellRange[0];

                    var x = colIndex * cellWidth + left;

                    ctx.beginPath();
                    ctx.moveTo(x, drawConsts.top);
                    ctx.lineTo(x, actualPlotHeight);
                    ctx.stroke();
                } // if
            } // for

            ctx.restore();

        });
    }); // get the cell order
}

/**
 * Make a cell selection from metadata
 * @param metadataName name of the metdata data entry on the basis of which to select (e.g clusters or batch)
 * @param metadataValue the value we are looking for (e.g. cluster 1, batch 2)
 * @param selectionName name to give to the new selection, assumed not to exist already
 * @param focus logical, give focus to the selection in the cell selection pane afterwards?
 * @param highlight logical, hightlight the selection on the heatmaps and embedding?
 */
metaDataHeatmapViewer.prototype.makeCellSelectionFromMetadata = function(metadataName, metadataValue, selectionName, focus, highlight, restriction) {
    // Generate a cell selection

    if (typeof restriction === "undefined") {
        restriction = function(selection) {
            return true;
        };
    }

    var dataCntr = new dataController();
    dataCntr.getCellMetadata(function(data, callbackParameters) {

        var val = callbackParameters.metadataValue;
        var cellSelectionNames = [];

        var keys = Object.keys(data[callbackParameters.metadataName].data);

        for (var kn = 0; kn < keys.length; kn++) {
            var cellid = keys[kn];
            if (data[callbackParameters.metadataName].data[cellid] == val) {
                cellSelectionNames.push(cellid);
            }
        }

        var cellSel = new cellSelectionController();
        var selectionName = false;
        if (restriction(cellSelectionNames)) {
            selectionName = cellSel.setSelection(cellSelectionNames, callbackParameters.selectionName, {}, data[callbackParameters.metadataName].palette[val].substring(0, 7));
        }

        if (selectionName && highlight) {
            var heatView = new heatmapViewer();
            heatView.highlightCellSelectionByName(selectionName);

            var aspHeatView = new aspectHeatmapViewer();
            aspHeatView.highlightCellSelectionByName(selectionName);

            var metaHeatView = new metaDataHeatmapViewer();
            metaHeatView.highlightCellSelectionByName(selectionName);

            // Highlight on embedding
            var embCntr = new embeddingViewer();
            embCntr.highlightSelectionByName(selectionName);

        }

    }, {
        metadataName: metadataName,
        metadataValue: metadataValue,
        selectionName: selectionName,
        focus: focus,
        highlight: highlight
    });

}

metaDataHeatmapViewer.prototype.makeAllCellSelectionsFromMetadata = function(metadataName, selNamePrefix, restriction) {
    // Generate a cell selection

    if (typeof restriction === "undefined") {
        restriction = function(selection) {
            return true;
        };
    }

    var dataCntr = new dataController();
    dataCntr.getCellMetadata(function(data, callbackParameters) {
        // data[callbackParameters.metadataName].data
        var cellSelections = [];

        var allCells = data[callbackParameters.metadataName].data;

        for (var cellid in allCells) {
            if (!cellSelections[allCells[cellid]]) {
                cellSelections[allCells[cellid]] = []
            }
            cellSelections[allCells[cellid]].push(cellid);
        }

        var cellSel = new cellSelectionController();
        for (var i = 0; i < cellSelections.length; i++) {
            var cellSelectionNames = cellSelections[i];
            if (cellSelectionNames && restriction(cellSelectionNames)) {
                cellSel.setSelection(cellSelectionNames, selNamePrefix + (i + 1), {}, data[callbackParameters.metadataName].palette[i].substring(0, 7));
            }
        }

    }, {
        metadataName: metadataName,
        selNamePrefix: selNamePrefix
    });
}

metaDataHeatmapViewer.prototype.makeAllCellSelectionsFromMetadataProgress = function(metadataName, selNamePrefix, restriction) {
    // Generate a cell selection

    if (typeof restriction === "undefined") {
        restriction = function(selection) {
            return true;
        };
    }

    var dataCntr = new dataController();
    dataCntr.getCellMetadata(function(data, callbackParameters) {
        // data[callbackParameters.metadataName].data
        var parameters = {};

        parameters.cellSelections = [];
        parameters.selNamePrefix = callbackParameters.selNamePrefix;
        parameters.selectionNames = (data[callbackParameters.metadataName].levels ? data[callbackParameters.metadataName].levels : data[callbackParameters.metadataName].palette);
        parameters.metadataName = callbackParameters.metadataName;
        parameters.restriction = restriction;
        var allCells = data[callbackParameters.metadataName].data;

        for (var cellid in allCells) {
            if (!parameters.cellSelections[allCells[cellid]]) {
                parameters.cellSelections[allCells[cellid]] = []
            }
            parameters.cellSelections[allCells[cellid]].push(cellid);
        }

        var stepFunction = function(params, i, step, max) {
            var cellSel = new cellSelectionController();
            for (var j = 0; j < step; j++) {
                var cellSelectionNames = params.cellSelections[i + j];
                if (cellSelectionNames && params.restriction(cellSelectionNames)) {
                    cellSel.setSelection(cellSelectionNames, params.selNamePrefix + params.selectionNames[i + j].split(/\ |\#/).join(""), {}, data[params.metadataName].palette[i].substring(0, 7), undefined, true);
                }
            }
        }

        var callback = function(params) {
            var cellSel = new cellSelectionController();
            cellSel.raiseSelectionChangedEvent();
        }

        pagHelpers.generateProgressBar(stepFunction, parameters.cellSelections.length, 1, callback, parameters);

    }, {
        metadataName: metadataName,
        selNamePrefix: selNamePrefix
    });

}

metaDataHeatmapViewer.prototype.generateLegend = function(metadataName) {
    var metaLegendStore = Ext.create("Ext.data.Store", {
        fields: [{
                name: 'index',
                type: 'string'
            },
            {
                name: 'color',
                type: 'string'
            },
        ],
        autoLoad: true
    })
    var dataCntr = new dataController();
    var metaName = "";
    dataCntr.getCellMetadata(function(data, callbackParameters) {
        var palette = data[callbackParameters.metadataName].palette
        metaName = data[callbackParameters.metadataName].displayname
        var display = palette;
        if (data[callbackParameters.metadataName].levels) {
            display = data[callbackParameters.metadataName].levels;
        }
        for (var i = 0; i < palette.length; i++) {
            metaLegendStore.add({
                index: display[i],
                color: palette[i]
            });
        }
    }, {
        metadataName: metadataName
    });
    var cellSelectionTable = Ext.create('Ext.grid.Panel', {
        title: "Legend: " + metaName,
        height: "100%",
        width: "100%",
        scrollable: true,
        store: metaLegendStore,
        columns: [{
                text: 'Cluster',
                dataIndex: 'index',
                width: '75%'
            },
            {
                text: "&#x03DF;",
                dataIndex: 'color',
                width: '25%',
                renderer: function(value, meta) {
                    meta.style = "background-color:" + value.substring(0, 7) + ";";
                },
            }
        ],
        emptyText: "Legend Not Available",
        singleSelect: false,
    });
    Ext.create("Ext.window.Window", {
        title: "Legend",
        modal: false,
        resizable: true,
        dragable: true,
        height: 300,
        width: 154,
        layout: "fit",
        items: [cellSelectionTable]

    }).show();
}

metaDataHeatmapViewer.prototype.downloadImage = function() {
    var canvas = document.getElementById('metadata-area');

    const maxSize = 2000;
    if (canvas.width > maxSize | canvas.height > maxSize) {
        Ext.Msg.show({
            title: 'Warning',
            msg: 'The current canvas size exceeds ' + maxSize + 'px in at least one dimention.' +
                'This may cause problems during exporting. Do you want to continue?',
            buttons: Ext.Msg.OKCANCEL,
            fn: function(s) {
                if (s == 'ok') {
                    canvas.toBlob(function(data) {
                        pagHelpers.downloadURL(data, 'metadata.png', canvas)
                    })
                } //if
            } //fn
        }) // Ext.Msg.show
    } else {
        canvas.toBlob(function(data) {
            pagHelpers.downloadURL(data, 'metadata.png', canvas)
        })
    }
}
