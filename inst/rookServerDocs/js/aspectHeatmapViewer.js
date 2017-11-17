"use strict";

/*
 * Filename: aspectHeatmapViewer.js
 * Author: Nikolas Barkas
 * Description: implements the aspect heatmap viewer for pagoda2
 */

/**
 * Object that manages the aspect heatmap viewer
 * @constructor
 */
function aspectHeatmapViewer() {
    if (typeof aspectHeatmapViewer.instance === 'object') {
        return aspectHeatmapViewer.instance;
    }

    // Handle extjs resize
    var extJsContainer = Ext.getCmp('aspectPanel');
    extJsContainer.onResize = function() {
        var o = new aspectHeatmapViewer();
        o.updateCanvasSize();
        o.drawHeatmap();
    };

    // Keep track of what selection we are showing so
    // we can persist accross redraws
    this.currentOverlaySelectionName = null;
    this.currentOverlaySelectionNames = null;
    this.currentOverlaySelectionShown = false;

    this.palManager = new paletteManager();
    this.palManager.setPalette(p2globalParams.aspectViewer.defaultPaletteName);
    this.palManager.setNumberOfColors(p2globalParams.aspectViewer.defaultPaletteLevels);

    aspectHeatmapViewer.instance = this;
}

/**
 * Generate the palettes menu
 * @private
 * @returns palette menu extjs object
 */
aspectHeatmapViewer.prototype.generatePalettesMenu = function() {
    var paletteChangeHandler = function(item) {
        var aspHeatView = new aspectHeatmapViewer();

        aspHeatView.palManager.setPalette(item.value);

        // NOTE: WE are getting  the number of colors because the
        // Manger will have sorted out any issues with exceeing the
        // new palette limits
        var curNoColours = aspHeatView.palManager.getNumberOfColors();

        // Set the actual value to the menu
        Ext.getCmp('aspectspaletteslevelsfield').setValue(curNoColours);

        // Impose the new limits of this palette on the extjs control
        Ext.getCmp('aspectspaletteslevelsfield').setMinValue(aspHeatView.palManager.getMinNumberOfColors());
        Ext.getCmp('aspectspaletteslevelsfield').setMaxValue(aspHeatView.palManager.getMaxNumberOfColors());

        aspHeatView.drawHeatmap();
    };


    var palettes = p2globalParams.heatmapViewer.availablePalettes; // CHANGE
    var paletteMenu = Ext.create('Ext.menu.Menu');
    for (var i in palettes) {
        paletteMenu.add({
            text: palettes[i].displayName,
            value: palettes[i].name,
            handler: paletteChangeHandler
        }); // paletteMenu.add
    } // for
    return paletteMenu;
}

/**
 * Generate the settings menu for the heatmap
 * @private
 */
aspectHeatmapViewer.prototype.generateAspectHeatmapSettingsMenu = function() {

    var paletteMenu = this.generatePalettesMenu();
    var aspHeatView = new aspectHeatmapViewer();

    return Ext.create('Ext.menu.Menu', {
        id: 'aspectSettingsMenu',
        items: [{
                text: 'Palette Name',
                menu: paletteMenu

            },
            {
                fieldLabel: 'Palette Levels',
                id: 'aspectspaletteslevelsfield',
                xtype: 'numberfield',
                tooltip: 'Number of colors for the palette',
                value: p2globalParams.aspectViewer.defaultPaletteLevels, // FIXME
                disabled: false,
                maxValue: aspHeatView.palManager.getMaxNumberOfColors(),
                minValue: aspHeatView.palManager.getMinNumberOfColors(),
                listeners: {
                    change: {
                        buffer: 800,
                        fn: function(f, v) {
                            var aspView = new aspectHeatmapViewer();
                            aspView.palManager.setNumberOfColors(v);
                            aspView.drawHeatmap();

                        }
                    } // buffer of change listener
                }
            } // numberfield
        ] // items

    });
}

/**
 * Get the area height
 */
aspectHeatmapViewer.prototype.getHeight = function() {
    return Ext.getCmp('aspectPanel').getHeight() - 10;
}

/**
 * Get the area width
 */
aspectHeatmapViewer.prototype.getWidth = function() {
    return (Ext.getCmp('aspectPanel').getWidth());
}

/**
 * Perform initialization of the aspect heatmap viewer
 * @description This is called by the parent heatmapDendrogram object
 * when initialization has finished.
 */
aspectHeatmapViewer.prototype.initialize = function(callback) {
    // Clickable regions for cross hairs and navigating
    this.aspectRegions = new clickableRegions();

    var aspectHeatmapContainer = $('#aspect-heatmap-container');
    aspectHeatmapContainer.css({
        position: 'relative'
    });

    aspectHeatmapContainer.append(
        '<canvas id="aspect-heatmap-area"></canvas>' +
        '<canvas id="aspect-heatmap-area-selection"></canvas>' +
        '<canvas id="aspect-heatmap-area-overlay"></canvas>'
    );

    var aspectHeatmapArea = $('#aspect-heatmap-area');
    aspectHeatmapArea.css({
        position: 'absolute',
        top: 0,
        left: 0
    });

    var aspectHeatmapAreaSelection = $('#aspect-heatmap-area-selection');
    aspectHeatmapAreaSelection.css({
        position: 'absolute',
        top: 0,
        left: 0
    })

    var aspectHeatmapAreaOverlay = $('#aspect-heatmap-area-overlay');
    aspectHeatmapAreaOverlay.css({
        position: 'absolute',
        top: 0,
        left: 0
    });

    // Setup events for overlays
    this.setupOverlays();

    // update the size of both canvases
    this.updateCanvasSize();

    // TODO: Update this with an apsect specific palette
    /*this.palManager = new paletteManager();
    this.palManager.setPalette(p2globalParams.aspectViewer.defaultPaletteName);
    this.palManager.setNumberOfColors(p2globalParams.aspectViewer.defaultPaletteLevels);*/

    // Make the menu
    //this.generateMenu();

// Callback from here for init load
    // Draw the heatmap
    this.drawHeatmap(callback);

};

/**
 * Setup the event listeners for the overlay effects
 */
aspectHeatmapViewer.prototype.setupOverlays = function() {
    var heatmapOverlayArea = $('#aspect-heatmap-area-overlay')[0];
    var aspHeatView = this;
    this.primaryMouseButtonDown = false;
    this.dragging = false;
    this.dragStartX = null;

    // For preventing selection on double click
    heatmapOverlayArea.addEventListener('mousedown', function(e) {
        e.preventDefault();
        var heatView = new aspectHeatmapViewer();
        var drawConsts = heatView.getDrawConstants();
        if (e.offsetX > drawConsts.left & e.offsetX < drawConsts.left + drawConsts.width & e.which == 1) {
            heatView.primaryMouseButtonDown = true;
            heatView.dragStartX = e.offsetX;
        }
    });

    heatmapOverlayArea.addEventListener('mousemove', function(e) {
        var aspHeatView = new aspectHeatmapViewer();
        var heatV = new heatmapViewer();
        var metaV = new metaDataHeatmapViewer();
        var heatDendView = new heatmapDendrogramViewer();
        var dendV = new dendrogramViewer();
        var embV = new embeddingViewer();

        var x = e.offsetX;
        var y = e.offsetY;

        var drawConsts = aspHeatView.getDrawConstants();

        // Calculate cell index for embedding hover
        var metaWidth = drawConsts.width - heatDendView.getPlotAreaRightPadding();
        var posPC = (e.offsetX - drawConsts.left) / metaWidth;
        if (posPC > 0 && posPC < 1) {
            var curDisplayIdxs = dendV.getCurrentDisplayCellsIndexes();
            var cellindex = Math.floor(posPC * (curDisplayIdxs[1] - curDisplayIdxs[0])) + curDisplayIdxs[0];
            embV.highlightCellByIndex(cellindex);
        } else {
            embV.clearHighlightCell();
        }

        heatV.showOverlay(x);
        metaV.showOverlay(x);
        aspHeatView.showOverlay(x, y);

        if (aspHeatView.primaryMouseButtonDown) {
            if (!aspHeatView.dragging) {
                // The first mouse move after the mouse down
                // Initiate dragging process
                aspHeatView.clearSelectionOverlay(); // This is for resetting the current selection params not for the actual clear
                aspHeatView.dragging = true;
            }

            // Clear the canvas
            var canvas = document.getElementById('aspect-heatmap-area-selection');
            var ctx = canvas.getContext('2d');
            var width = canvas.width;
            var height = canvas.height;
            ctx.clearRect(0, 0, width, height);


            var actualPlotHeight = aspHeatView.getActualPlotHeight();



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
            ctx.fillRect(aspHeatView.dragStartX, drawConsts.top, boundedX - aspHeatView.dragStartX, actualPlotHeight);
            ctx.restore();
        }

    });

    heatmapOverlayArea.addEventListener('mouseenter', function(e) {
        document.body.style.cursor = "crosshair";
    });

    heatmapOverlayArea.addEventListener('mouseout', function(e) {
        var aspHeatView = new aspectHeatmapViewer();
        var metaV = new metaDataHeatmapViewer();
        var heatV = new heatmapViewer();

        aspHeatView.clearOverlay();
        metaV.clearOverlay();
        heatV.clearOverlay();
        document.body.style.cursor = "default";
    });

    heatmapOverlayArea.addEventListener('mouseup', function(e) {

        var heatDendView = new heatmapDendrogramViewer();

        var aspHeatView = new aspectHeatmapViewer();

        if (aspHeatView.primaryMouseButtonDown) {
            aspHeatView.primaryMouseButtonDown = false;
            if (aspHeatView.dragging) {
                // End of drag
                aspHeatView.dragging = false;

                // Range of X is aspHeatView.dragStartX  to e.offsetX

                var drawConsts = aspHeatView.getDrawConstants();

                var dendV = new dendrogramViewer();
                var curDisplayIdxs = dendV.getCurrentDisplayCellsIndexes();


                var metaWidth = drawConsts.width - heatDendView.getPlotAreaRightPadding();


                // Start and end as percent of current display cell range
                var startPC = (aspHeatView.dragStartX - drawConsts.left) / metaWidth;
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
                var heatView = new heatmapViewer();
                heatView.highlightCellSelectionByName(selectionName);

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
                var aspHeatView = new aspectHeatmapViewer();
                var regionData = aspHeatView.aspectRegions.resolveClick(x, y);
                // Draw tooltip
                // Tell the embedding to update
                var aspTable = new aspectsTableViewer();
                aspTable.showSelectedAspect(regionData.aspectId);

                var embV = new embeddingViewer();
                embV.setColorConfiguration('aspect');
                embV.setAspectColorInfo({
                    aspectid: regionData.aspectId
                });
                embV.updateColors();
            }
        }
    });


};

/**
 * Clear the overlay
 */
aspectHeatmapViewer.prototype.clearOverlay = function() {
    var overlayArea = document.getElementById('aspect-heatmap-area-overlay');
    var ctx = overlayArea.getContext('2d');
    var width = overlayArea.width;
    var height = overlayArea.height;
    ctx.clearRect(0, 0, width, height);
}

aspectHeatmapViewer.prototype.clearSelectionOverlay = function() {
    this.clearSelectionOverlayInternal();
    this.currentOverlaySelectionShown = false;
}

aspectHeatmapViewer.prototype.clearSelectionOverlayInternal = function() {
    var canvas = document.getElementById('aspect-heatmap-area-selection');
    var ctx = canvas.getContext('2d');
    var height = canvas.height;
    var width = canvas.width;
    ctx.clearRect(0, 0, width, height);
}


/**
 * Show overlay for specific coordinates
 */
aspectHeatmapViewer.prototype.showOverlay = function(x, y) {
    var aspHeatView = new aspectHeatmapViewer()

    var heatDendView = new heatmapDendrogramViewer();
    var overlayArea = document.getElementById('aspect-heatmap-area-overlay');
    var ctx = overlayArea.getContext('2d');

    var drawConsts = aspHeatView.getDrawConstants();

    var areaWidth = overlayArea.width;
    var areaHeight = overlayArea.height;

    ctx.setLineDash([10, 10])
    ctx.lineWidth = 1;
    ctx.clearRect(0, 0, areaWidth, areaHeight);

    var actualPlotHeight = this.getActualPlotHeight();

    /* if (typeof y !== 'undefined' & y < actualPlotHeight & y > drawConsts.top){
      ctx.beginPath();
      ctx.moveTo(drawConsts.left, y);
      ctx.lineTo(drawConsts.width + drawConsts.left, y);
      ctx.stroke();
    }
    */

    if (typeof x !== 'undefined' & x > drawConsts.left & x < drawConsts.width + drawConsts.left - heatDendView.getPlotAreaRightPadding() &
        (y < actualPlotHeight | typeof y === 'undefined') // if y is provided it is in the plot
    ) {
        ctx.beginPath();
        ctx.moveTo(x, drawConsts.top);
        ctx.lineTo(x, actualPlotHeight + drawConsts.top);
        ctx.stroke();
    }

}

/**
 * Get the plot height that was actually used
 */
aspectHeatmapViewer.prototype.getActualPlotHeight = function() {
    return this.actualPlotHeight;
}

/**
 * Set the plot height that was actually used
 */
aspectHeatmapViewer.prototype.setActualPlotHeight = function(val) {
    this.actualPlotHeight = val;
}

/**
 * Update the canvas size to that provided by the heatmapDendrogramViewer
 */
aspectHeatmapViewer.prototype.updateCanvasSize = function() {


    var aspectHeatmapArea = $('#aspect-heatmap-area')[0];
    var aspectHeatmapAreaOverlay = $('#aspect-heatmap-area-overlay')[0];

    // Get and store current height
    var heatDendV = new heatmapDendrogramViewer();
    var curWidth = this.getWidth();
    var curHeight = this.getHeight();

    var containerDiv = $('#aspect-heatmap-container');
    containerDiv.css({
        'width': curWidth,
        'height': curHeight
    });

    this.canvasElementWidth = curWidth;
    this.canvasElementHeight = curHeight;

    // Update the size of teh main area
    aspectHeatmapArea.width = curWidth;
    aspectHeatmapArea.height = curHeight;

    // Update the size of the overaly
    aspectHeatmapAreaOverlay.width = curWidth;
    aspectHeatmapAreaOverlay.height = curHeight;


    // Resize the selection canvas
    var heatmapAreaSelection = $('#aspect-heatmap-area-selection')[0];
    heatmapAreaSelection.width = curWidth;
    heatmapAreaSelection.height = curHeight;
}

/**
 * Clear the heatmap
 */
aspectHeatmapViewer.prototype.clearHeatmap = function(ctx) {
    ctx.clearRect(0, 0, this.width, this.height);
}

/**
 * Draw the heatmap. Will clear heatmap if required
 */
aspectHeatmapViewer.prototype.drawHeatmap = function(callback) {
    var aspHeatView = this;
    var heatDendView = new heatmapDendrogramViewer();
    var dendV = new dendrogramViewer();
    var ctx = this.getDrawingContext();

    var drawConsts = this.getDrawConstants();
    var top = drawConsts.top;
    var left = drawConsts.left;
    var heatmapWidth = drawConsts.width - heatDendView.getPlotAreaRightPadding();
    var heatmapHeight = drawConsts.height - drawConsts.paddingBottom;

    // Get the cells to plot
    var cellRange = dendV.getCurrentDisplayCellsIndexes();
    var cellIndexStart = cellRange[0];
    var cellIndexEnd = cellRange[1];

    this.clearHeatmap(ctx);


    // Show centered waiting icon
    $('#aspect-heatmap-container').append("<img class='loadingIcon' src='img/loading.gif'/>");
    var loadingDomItem = $('#aspect-heatmap-container > .loadingIcon');
    var lpad = this.getWidth() / 2;
    var tpad = this.getHeight() / 2;
    loadingDomItem.css({
        'padding-left': lpad + 'px',
        'padding-top': tpad + 'px'
    });


    var dataCntr = new dataController();
    dataCntr.getAspectMatrix(cellIndexStart, cellIndexEnd, false, function(data) {
        loadingDomItem.remove();

        var naspects = data.Dim[1];
        var ncells = data.Dim[0];

        // Computed plotting params
        var cellWidth = heatmapWidth / ncells;
        var cellHeight = heatmapHeight / naspects;

        var actualPlotHeight = heatmapHeight; // for convinience
        aspHeatView.setActualPlotHeight(actualPlotHeight);

        var palSize = aspHeatView.palManager.getNumberOfColors();
        var pal = aspHeatView.palManager.getPaletteColors();


        ctx.fillStyle = pal[Math.floor(palSize / 2)]
        ctx.fillRect(left, top, heatmapWidth, actualPlotHeight);

        for (var j = 0; j < data.p.length - 1; j++) {
            var rsi = data.p[j];
            var rei = data.p[j + 1] - 1;

            if (rsi === rei) {
                continue;
            };

            // Calculate row normalisation
            var rowMin = data.x.slice(rsi, rei).reduce(function(a, b) {
                return Math.min(a, b)
            });
            var rowMax = data.x.slice(rsi, rei).reduce(function(a, b) {
                return Math.max(a, b)
            });
            var rowSum = data.x.slice(rsi, rei).reduce(function(a, b) {
                return a + b
            });
            var rowMean = rowSum / (rei - rsi + 1);
            var maxAbsValue = Math.max(Math.abs(rowMin - rowMean), Math.abs(rowMax - rowMean));

            // Color mapper for this row
            var colorMapper = aspHeatView.palManager.getMeanClampedColorMapper(rowMean, maxAbsValue, palSize);

            for (var k = rsi; k < rei; k++) {
                var palIndex = colorMapper(data.x[k]);
                ctx.fillStyle = pal[palIndex];

                var x = data.i[k] * cellWidth + left;
                var y = j * cellHeight + top;

                ctx.fillRect(x, y, cellWidth, cellHeight);
            } // for k


        } // for j

        // Plot the bounding box
        ctx.strokeRect(left, top, heatmapWidth, actualPlotHeight);

        // Plot the labels
        for (var i = 0; i < data.DimNames2.length; i++) {
            // Print the names
            var name = data.DimNames2[i];
            // Get the appropriate font size for this number of cells
            var fontSize = aspHeatView.getRowFontSize(cellHeight)

            // Calculate position
            x = ncells * cellWidth + left + 10;
            y = i * cellHeight + top + cellHeight / 2 + fontSize / 3;

            // Plot
            ctx.font = fontSize + 'px Arial';
            ctx.fillStyle = 'black';
            ctx.fillText(name, x, y);
        } // Label plot loop

        // TODO: setup click areas here
        aspHeatView.aspectRegions.clearClickAreas();
        for (var i = 0; i < data.DimNames2.length; i++) {
            var x1 = left;
            var y1 = cellHeight * i + top;
            var x2 = heatmapWidth;
            var y2 = cellHeight * (i + 1) + top;

            aspHeatView.aspectRegions.addClickArea(
                x1, y1,
                x1, y2,
                x2, y2,
                x2, y1, {
                    aspectId: data.DimNames2[i]
                }
            );
        }

        aspHeatView.clearSelectionOverlayInternal();
        if (aspHeatView.currentOverlaySelectionShown === true) {
            if (aspHeatView.currentOverlaySelectionName !== null) {
                aspHeatView.highlightCellSelectionByName(aspHeatView.currentOverlaySelectionName);
            } else {
                aspHeatView.highlightCellSelectionsByNames(aspHeatView.currentOverlaySelectionNames);
            }
        }
      
    }); // dataController callback

};


/**
 * Get 2d drawing context for the canvas
 */
aspectHeatmapViewer.prototype.getDrawingContext = function() {
    return document.getElementById('aspect-heatmap-area').getContext('2d');
}

aspectHeatmapViewer.prototype.getSelectionDrawingContext = function() {
    var canvas = document.getElementById('aspect-heatmap-area-selection');
    var ctx = canvas.getContext('2d');
    return ctx;
}


/**
 * Get drawing constants for drawing
 */
aspectHeatmapViewer.prototype.getDrawConstants = function() {
    var heatDendView = new heatmapDendrogramViewer();

    // TODO: values here should be in global params
    return {
        top: 5,
        left: heatDendView.getPlotAreaLeftPadding(),
        width: this.getWidth(),
        height: this.getHeight(),
        paddingBottom: 10,
        maxCellHeight: 30,
    }
}

aspectHeatmapViewer.prototype.highlightCellSelectionByName = function(selectionName) {
    var aspHeatView = this;
    var dendV = new dendrogramViewer();

    this.currentOverlaySelectionName = selectionName;
    this.currentOverlaySelectionNames = null;
    this.currentOverlaySelectionShown = true;

    var heatDendView = new heatmapDendrogramViewer();

    var cellSelCntr = new cellSelectionController();
    var cellSelection = cellSelCntr.getSelection(selectionName);

    var dataCntr = new dataController();
    dataCntr.getCellOrderHash(function(cellorderHash) {
        var cellRange = dendV.getCurrentDisplayCellsIndexes();
        var ncells = cellRange[1] - cellRange[0];

        var ctx = aspHeatView.getSelectionDrawingContext();
        ctx.clearRect(0, 0, 3000, 3000);

        // Get and calculate plotting values
        var drawConsts = aspHeatView.getDrawConstants();
        var heatmapWidth = drawConsts.width - heatDendView.getPlotAreaRightPadding();
        var cellWidth = heatmapWidth / ncells;
        var left = drawConsts.left;
        var n = cellSelection.length;

        var actualPlotHeight = aspHeatView.getActualPlotHeight() + 6;


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

    })

}

aspectHeatmapViewer.prototype.getRowFontSize = function(cellHeight) {
    var a = Math.min(cellHeight, 11);
    a = a * 3 / 4;
    if (a < 4) {
        a = 0;
    }

    return a;
}

aspectHeatmapViewer.prototype.highlightCellSelectionsByNames = function(selectionNames) {
    var aspHeatView = this;
    var dendV = new dendrogramViewer();

    this.currentOverlaySelectionNames = selectionNames;
    this.currentOverlaySelectionName = null;
    this.currentOverlaySelectionShown = true;

    var heatDendView = new heatmapDendrogramViewer();

    var cellSelCntr = new cellSelectionController();
    var ctx = aspHeatView.getSelectionDrawingContext();
    ctx.clearRect(0, 0, 3000, 3000);
    var dataCntr = new dataController();


    dataCntr.getCellOrder(function(cellorder) {
        selectionNames.forEach(function(selectionName) {
            var cellSelection = cellSelCntr.getSelection(selectionName);
            var cellRange = dendV.getCurrentDisplayCellsIndexes();
            var ncells = cellRange[1] - cellRange[0];

            // Get and calculate plotting values
            var drawConsts = aspHeatView.getDrawConstants();
            var heatmapWidth = drawConsts.width - heatDendView.getPlotAreaRightPadding();
            var cellWidth = heatmapWidth / ncells;
            var left = drawConsts.left;
            var n = cellSelection.length;

            var actualPlotHeight = aspHeatView.getActualPlotHeight() + 6;


            ctx.save();
            var selColor = pagHelpers.hexToRgb(cellSelCntr.getColor(selectionName));
            ctx.strokeStyle = 'rgba(' + selColor.r + ',' + selColor.g + ',' + selColor.b + ',0.2)';

            // Draw vertical lines for selected cells
            for (var i = 0; i < n; i++) {
                var cellIndex = cellorder.indexOf(cellSelection[i]);

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
        })
    })

}

aspectHeatmapViewer.prototype.downloadImage = function() {
    var canvas = document.getElementById('aspect-heatmap-area');

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
                        pagHelpers.downloadURL(data, 'aspects.png', canvas)
                    })
                } //if
            } //fn
        }) // Ext.Msg.show
    } else {
        canvas.toBlob(function(data) {
            pagHelpers.downloadURL(data, 'aspects.png', canvas)
        })
    }
}
