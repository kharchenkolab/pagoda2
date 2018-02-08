"use strict";

/*
 * Filename: embeddingViewerScatterCanvas.js
 * Author: Nikolas Barkas
 * Date: March 2017
 * Description: Implements scatter plot embeding for pagoda2 using canvas
 */

/**
 * Plots the scatter plot using canvas
 * @constructor
 */
function embeddingViewerScatterCanvas() {
    var element = Ext.getCmp('embedding-app-container');
    element.onResize = function() {
        var embView = new embeddingViewer();
        embView.plotEmbedding();
    };

    // This variable hold the last ajax request for expression values
    // The purpose of this is to be able to cancel this request
    // If the user makes a new request while the current one is running
    this.colorAJAXrequest;

    // We use this to cache the cell order as calculated by getExpression
    // and avoid recalculating every time. We need to cache this for
    // each embedding seperately
    this.fillStylesGeneExpressionOrder = {};

    // Used to slightly shrink the plotting area to prevent
    // plotting on the edges
    this.rangeScaleFactor = 0.97;

    this.domainCacheId = null;

    // These are used to keep track of the
    // Transformation applied to the embedding points
    // This is needed so that selections can be made
    this.xScaleDomainMin;
    this.xScaleDomainMax;
    this.xScaleRangeMin;
    this.xScaleRangeMax;

    this.yScaleDomainMin;
    this.yScaleDomainMax;
    this.yScaleRangeMin;
    this.yScaleRangeMax;
    this.highlight = "box";

    this.cellPositionCacheByID = {};
    this.cellPositionCacheByIndex = {};

    this.indexCacheBuilding = false;

    // Generates the html structure required for the viewer
    this.generateStructure();
}

/**
 * Returns the main canvas element for this embedding view
 */
embeddingViewerScatterCanvas.prototype.getMainCanvasElement = function() {
    return document.getElementById('embedding-canvas');
}

embeddingViewerScatterCanvas.prototype.calculateDomain = function(plotData, type, embeddingType) {
    // Calculate the scale domains if required
    var cacheId = type + '_' + embeddingType;
    if (this.domainCacheId !== cacheId) {
        this.xScaleDomainMin = +Infinity;
        this.xScaleDomainMax = -Infinity;
        this.yScaleDomainMin = +Infinity;
        this.yScaleDomainMax = -Infinity;
        for (var j = 0; j < plotData.length; j++) {
            this.xScaleDomainMin = Math.min(this.xScaleDomainMin, plotData[j][1]);
            this.xScaleDomainMax = Math.max(this.xScaleDomainMax, plotData[j][1]);
            this.yScaleDomainMin = Math.min(this.yScaleDomainMin, plotData[j][2]);
            this.yScaleDomainMax = Math.max(this.yScaleDomainMax, plotData[j][2]);
        }
        this.domainCacheId = cacheId;
    }
}

/**
 * Generate the selection following the completion of the
 * drag event.
 * @description Perform the actions necessary at the end of
 * the drag event completion. Specifically, clear the overlay
 * plot only the selected points onto the overlay
 * and set the cell selection
 */
embeddingViewerScatterCanvas.prototype.generateDragSelection =
    function(vertices) {
        var thisViewer = this;
        var embViewer = new embeddingViewer();
        var config = embViewer.getConfig();

        var dataCntr = new dataController();
        var type = config.type;
        var embeddingType = config.embeddingType;

        var size = this.size;

        dataCntr.getEmbedding(type, embeddingType, function(plotData) {
            thisViewer.calculateDomain(plotData, type, embeddingType);

            // Make the scales
            thisViewer.xScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
            thisViewer.xScaleRangeMax = size * thisViewer.rangeScaleFactor;
            var xScale = pagHelpers.linearScaleGenerator(thisViewer.xScaleDomainMin,
                thisViewer.xScaleDomainMax,
                thisViewer.xScaleRangeMin,
                thisViewer.xScaleRangeMax, thisViewer.hpad);
            thisViewer.yScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
            thisViewer.yScaleRangeMax = size * thisViewer.rangeScaleFactor;
            var yScale = pagHelpers.linearScaleGenerator(thisViewer.yScaleDomainMin,
                thisViewer.yScaleDomainMax,
                thisViewer.yScaleRangeMin,
                thisViewer.yScaleRangeMax, thisViewer.vpad);

            var pointsize = embViewer.getCurrentPointSize();

            // Clear the hover canvas (contains the rectangle)
            var canvasHover = document.getElementById('embedding-canvas-hover');
            var ctxHover = canvasHover.getContext('2d');
            ctxHover.clearRect(0, 0, canvasHover.width, canvasHover.height);

            // Clear the overlay canvas
            var canvas = document.getElementById('embedding-canvas-overlay');
            var ctx = canvas.getContext('2d');
            ctx.clearRect(0, 0, canvas.width, canvas.height);

            var cellsForSelection = new Array();
            ctx.strokeStyle = 'red';

            // Plot hightlight
            const PI_2 = 2 * Math.PI;

            for (var i = 0; i < plotData.length; i++) {
                var point = plotData[i];
                var xs = xScale(point[1]);
                var ys = yScale(point[2]);
                if (pointInPolygon([xs, ys], vertices)) {
                    // Point in selection
                    cellsForSelection.push(point[0]);
                    ctx.beginPath();
                    ctx.arc(xs, ys, pointsize, 0, PI_2, false);
                    ctx.stroke();
                }
            }

            var cellSelCntr = new cellSelectionController();
            var selectionName = cellSelCntr.setSelection(cellsForSelection, 'Embedding Selection', new Object(), '#ff0000', 'embSelection');

            var heatView = new heatmapViewer();
            heatView.highlightCellSelectionByName(selectionName);

            var aspHeatView = new aspectHeatmapViewer();
            aspHeatView.highlightCellSelectionByName(selectionName);

            var metaHeatView = new metaDataHeatmapViewer();
            metaHeatView.highlightCellSelectionByName(selectionName);

        });
    }

/**
 * Highlight cells by selection name
 * @param selectionName the name of the cell selection as registered int he cellSelectionController
 * @param hasLabels Whether or not the name of the cell selection should be written on top of the highlighted cell selection
 */
embeddingViewerScatterCanvas.prototype.highlightSelectionByName = function(selectionName, hasLabels) {
    var cellSelCntr = new cellSelectionController();
    var cells = cellSelCntr.getSelection(selectionName);

    var embViewer = new embeddingViewer();
    var thisViewer = this;

    var config = embViewer.getConfig();
    var dataCntr = new dataController();
    var type = config.type;
    var embeddingType = config.embeddingType;

    var size = this.size;

    dataCntr.getEmbedding(type, embeddingType, function(data) {
        var plotData = data;

        thisViewer.calculateDomain(plotData, type, embeddingType);

        thisViewer.xScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
        thisViewer.xScaleRangeMax = size * thisViewer.rangeScaleFactor;
        var xScale = pagHelpers.linearScaleGenerator(thisViewer.xScaleDomainMin,
            thisViewer.xScaleDomainMax,
            thisViewer.xScaleRangeMin,
            thisViewer.xScaleRangeMax, thisViewer.hpad);


        thisViewer.yScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
        thisViewer.yScaleRangeMax = size * thisViewer.rangeScaleFactor;
        var yScale = pagHelpers.linearScaleGenerator(thisViewer.yScaleDomainMin,
            thisViewer.yScaleDomainMax,
            thisViewer.yScaleRangeMin,
            thisViewer.yScaleRangeMax, thisViewer.vpad);

        var pointsize = embViewer.getCurrentPointSize();

        var canvas = document.getElementById('embedding-canvas-overlay');
        var ctx = canvas.getContext('2d');
        (new embeddingViewer()).currentViewer.highlight
        ctx.save();
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        thisViewer.hasLabels = hasLabels;
        ctx.strokeStyle = cellSelCntr.getColor(selectionName);

        var clusterCenter = {
            x: 0,
            y: 0,
            total: 0,
        }

        const PI_2 = 2 * Math.PI;
        for (var i = 0; i < plotData.length; i++) {
            var point = plotData[i];
            if (cells.indexOf(point[0]) > -1) {
                var xs = xScale(point[1]);
                var ys = yScale(point[2]);

                ctx.beginPath();
                clusterCenter.x += xs;
                clusterCenter.y += ys;
                clusterCenter.total++;
                ctx.arc(xs, ys, pointsize, 0, PI_2, false);
                ctx.stroke();
            }
        } // for
        if (clusterCenter.total > 0) {
            ctx.fillStyle = "black";
            ctx.font = "bold " + ctx.font;
            ctx.textAlign = "center";
            ctx.textBaseline = "middle";
            ctx.fillText(cellSelCntr.getSelectionDisplayName(selectionName), clusterCenter.x / clusterCenter.total, clusterCenter.y)
        }
        ctx.restore();
    });
}

/**
 * Highlight cells by selection name
 * @param selectionNames the names of the cell selections as registered int he cellSelectionController
 * @param hasLabels Whether or not the name of the cell selection should be written on top of the highlighted cell selection
 */
embeddingViewerScatterCanvas.prototype.highlightSelectionsByNames = function(selectionNames, hasLabels) {
    var cellSelCntr = new cellSelectionController();
    var embViewer = new embeddingViewer();
    var thisViewer = this;
    thisViewer.hasLabels = hasLabels;

    var config = embViewer.getConfig();
    var dataCntr = new dataController();
    var type = config.type;
    var embeddingType = config.embeddingType;

    var size = this.size;

    var canvas = document.getElementById('embedding-canvas-overlay');
    var ctx = canvas.getContext('2d');
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    dataCntr.getEmbedding(type, embeddingType, function(data) {
        var plotData = data;

        thisViewer.calculateDomain(plotData, type, embeddingType);

        // Make the xscale
        thisViewer.xScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
        thisViewer.xScaleRangeMax = size * thisViewer.rangeScaleFactor;
        var xScale = pagHelpers.linearScaleGenerator(thisViewer.xScaleDomainMin,
            thisViewer.xScaleDomainMax,
            thisViewer.xScaleRangeMin,
            thisViewer.xScaleRangeMax, thisViewer.hpad);

        // Make the y scale
        thisViewer.yScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
        thisViewer.yScaleRangeMax = size * thisViewer.rangeScaleFactor;
        var yScale = pagHelpers.linearScaleGenerator(thisViewer.yScaleDomainMin,
            thisViewer.yScaleDomainMax,
            thisViewer.yScaleRangeMin,
            thisViewer.yScaleRangeMax, thisViewer.vpad);

        var pointsize = embViewer.getCurrentPointSize();

        var clusterLabels = [];
        selectionNames.forEach(function(selectionName) {

            var cells = cellSelCntr.getSelection(selectionName);
            ctx.save();
            ctx.strokeStyle = cellSelCntr.getColor(selectionName);
            var selData = {
                x: 0,
                y: 0,
                total: 0
            }
            for (var i = 0; i < plotData.length; i++) {
                var point = plotData[i];
                if (cells.indexOf(point[0]) > -1) {
                    var xs = xScale(point[1]);
                    var ys = yScale(point[2]);
                    selData.x += xs;
                    selData.y += ys;
                    selData.total++;
                    ctx.beginPath();
                    ctx.arc(xs, ys, pointsize, 0, 2 * Math.PI, false);
                    ctx.stroke();
                }
            } // for
            if (selData.total !== 0) {
                clusterLabels.push({
                    x: selData.x / selData.total,
                    y: selData.y / selData.total,
                    name: cellSelCntr.getSelectionDisplayName(selectionName)
                })
            }
            ctx.restore();
        });

        //adds label if it exists
        if (hasLabels) {
            ctx.save();
            ctx.fillStyle = "black";
            var fontPieces = ctx.font.split(/\s/);
            ctx.font = "bold " + Math.floor(new cellSelectionUIcontroller().selectionFont) + "px " + fontPieces[1];
            ctx.textAlign = "center";
            ctx.textBaseline = "middle";
            for (var i = 0; i < clusterLabels.length; i++) {
                ctx.fillText(clusterLabels[i].name, clusterLabels[i].x, clusterLabels[i].y);
            }
            ctx.restore();
        }
    });
}

/**
 * Highlight cells by selection name
 * @param overlayCanvas canvas target to draw scatter plot onto
 * @param size Square dimmension of the plot's dimmension
 * @param selectionNames the names of each selection to be highlighted
 */
embeddingViewerScatterCanvas.prototype.highlightSelectionsByNamesOntoCanvas = function(canvas, size, selectionNames) {

    var cellSelCntr = new cellSelectionController();
    var embViewer = new embeddingViewer();
    var thisViewer = this;

    var config = embViewer.getConfig();
    var dataCntr = new dataController();
    var type = config.type;
    var embeddingType = config.embeddingType;
    var ratio = size / this.size;
    var pointsize = embViewer.getCurrentPointSize() * ratio;

    var ctx = canvas.getContext('2d');
    ctx.clearRect(0, 0, canvas.width, canvas.height);


    dataCntr.getEmbedding(type, embeddingType, function(data) {
        var plotData = data;

        thisViewer.calculateDomain(plotData, type, embeddingType);

        // Make the xscale
        thisViewer.xScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
        thisViewer.xScaleRangeMax = size * thisViewer.rangeScaleFactor;
        var xScale = pagHelpers.linearScaleGenerator(thisViewer.xScaleDomainMin,
            thisViewer.xScaleDomainMax,
            thisViewer.xScaleRangeMin,
            thisViewer.xScaleRangeMax, thisViewer.hpad);

        // Make the y scale
        thisViewer.yScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
        thisViewer.yScaleRangeMax = size * thisViewer.rangeScaleFactor;
        var yScale = pagHelpers.linearScaleGenerator(thisViewer.yScaleDomainMin,
            thisViewer.yScaleDomainMax,
            thisViewer.yScaleRangeMin,
            thisViewer.yScaleRangeMax, thisViewer.vpad);

        ctx.save();
        var clusterLabels = [];
        selectionNames.forEach(function(selectionName) {
            var cells = cellSelCntr.getSelection(selectionName);
            ctx.strokeStyle = cellSelCntr.getColor(selectionName);
            var selData = {
                x: 0,
                y: 0,
                total: 0
            }
            for (var i = 0; i < plotData.length; i++) {
                var point = plotData[i];
                if (cells.indexOf(point[0]) > -1) {
                    var xs = xScale(point[1]);
                    var ys = yScale(point[2]);
                    selData.x += xs;
                    selData.y += ys;
                    selData.total++;
                    ctx.beginPath();
                    ctx.arc(xs, ys, pointsize, 0, 2 * Math.PI, false);
                    ctx.stroke();
                }
            } // for
            if (selData.total !== 0) {
                clusterLabels.push({
                    x: selData.x / selData.total,
                    y: selData.y / selData.total,
                    name: cellSelCntr.getSelectionDisplayName(selectionName)
                })
            }
            ctx.restore();
        });
        if (thisViewer.hasLabels) {
            ctx.save();
            ctx.fillStyle = "black";
            var fontPieces = ctx.font.split(/\s/);
            ctx.font = "bold " + ((new cellSelectionUIcontroller()).selectionFont * ratio) + "px " + fontPieces[1];
            ctx.textAlign = "center";
            ctx.textBaseline = "middle";
            for (var i = 0; i < clusterLabels.length; i++) {
                ctx.fillText(clusterLabels[i].name, clusterLabels[i].x, clusterLabels[i].y);
            }
            ctx.restore();
        }
    });

}

/**
 * Update the colors, delegated to draw()
 */
embeddingViewerScatterCanvas.prototype.updateColors = function() {
    this.draw();
}

/**
 * updateColors with gene expresion values, delegated to draw()
 */
embeddingViewerScatterCanvas.prototype.updateColorsGeneexpression = function() {
    this.draw();
}

/**
 * updateColors with gene metadata, delegated to draw()
 */
embeddingViewerScatterCanvas.prototype.updateColorsMetadata = function() {
    this.draw();
}

/**
 * plot the embedding from scratch, delegated to draw()
 */
embeddingViewerScatterCanvas.prototype.plotEmbedding = function() {
    this.draw();
}

/**
 * Download the current view with the overlay but not the hover
 */
embeddingViewerScatterCanvas.prototype.download = function() {
    var canvasOverlay = document.getElementById('embedding-canvas-overlay');
    var canvasMain = document.getElementById('embedding-canvas');

    var canvas = document.createElement('canvas');
    canvas.width = canvasOverlay.width;
    canvas.height = canvasOverlay.height;

    var ctx = canvas.getContext('2d');
    ctx.drawImage(canvasMain, 0, 0);
    ctx.drawImage(canvasOverlay, 0, 0);

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
                        pagHelpers.downloadURL(data, 'embedding.png', canvas)
                    })
                } //if
            } //fn
        }) // Ext.Msg.show
    } else {
        canvas.toBlob(function(data) {
            pagHelpers.downloadURL(data, 'embedding.png', canvas)
        })
    }
}

embeddingViewerScatterCanvas.prototype.setupOverlayEvents = function(overlayCanvasElement) {
    var thisViewer = this;

    thisViewer.dragging = false;
    var dragStartX = 0;
    var dragStartY = 0;
    var ctx = overlayCanvasElement.getContext('2d');

    var lastCursorPositionX = 0;
    var lastCursorPositionY = 0;
    var polygonVerts = [];
    thisViewer.stopRadius = 7;

    overlayCanvasElement.addEventListener('mousedown', function(e) {
        e.preventDefault();
        if (thisViewer.highlight === "box") {
            dragStartX = e.offsetX;
            dragStartY = e.offsetY;
            thisViewer.dragging = true;
        }

    });

    overlayCanvasElement.addEventListener('mouseup', function(e) {
        if (thisViewer.highlight === "box") {
            if (thisViewer.dragging) {
                // Dragging complete
                var dragEndX = e.offsetX;
                var dragEndY = e.offsetY;
                var vertices = [
                    [dragStartX, dragStartY],
                    [dragStartX, dragEndY],
                    [dragEndX, dragEndY],
                    [dragEndX, dragStartY]
                ];
                thisViewer.generateDragSelection(vertices);
            }
            thisViewer.dragging = false;
        }
    });

    overlayCanvasElement.addEventListener('click', function(e) {

        var xPos = e.offsetX;
        var yPos = e.offsetY;

        //sets initial position for polygon highlighting if poly highlight is selected or creates a new vertex
        if (thisViewer.highlight === "poly") {
            if (thisViewer.dragging) {

                if (Math.sqrt(Math.pow(xPos - polygonVerts[0][0], 2) + Math.pow(yPos - polygonVerts[0][1], 2)) < thisViewer.stopRadius) {
                    ctx.clearRect(0, 0, overlayCanvasElement.width, overlayCanvasElement.height);


                    thisViewer.generateDragSelection(polygonVerts);
                    polygonVerts = [];
                    thisViewer.dragging = false;
                    document.body.style.cursor = 'crosshair'
                } else {
                    polygonVerts.push([xPos, yPos]);
                }
            } else {
                thisViewer.dragging = true;
                polygonVerts.push([xPos, yPos]);
            }
        }

    });

    //changes cursor to crosshair on hover
    overlayCanvasElement.addEventListener('mouseover', function(e) {
        var xPos = e.offsetX;
        var yPos = e.offsetY;
        document.body.style.cursor = 'crosshair';

    });

    overlayCanvasElement.addEventListener('mousemove', function(e) {
        var xPos = e.offsetX;
        var yPos = e.offsetY;

        //handles polygon highlighting of the selected region
        if (thisViewer.highlight === "poly") {

            //if there is a dragging event going on, draws the lines enclosing the space
            if (thisViewer.dragging) {
                ctx.clearRect(0, 0, overlayCanvasElement.width, overlayCanvasElement.height);
                ctx.save();
                ctx.setLineDash([10, 10]);
                ctx.strokeStyle = 'rgba(255,0,0,1)';
                ctx.lineWidth = 2;
                ctx.beginPath();
                ctx.moveTo(polygonVerts[0][0], polygonVerts[0][1]);
                for (var i = 1; i < polygonVerts.length; i++) {
                    var next = polygonVerts[i];
                    ctx.lineTo(next[0], next[1]);
                }
                ctx.lineTo(xPos, yPos);
                ctx.stroke();
                ctx.closePath();

                ctx.beginPath()
                ctx.arc(polygonVerts[0][0], polygonVerts[0][1], thisViewer.stopRadius, 0, 2 * Math.PI, false);
                ctx.fillStyle = 'red';
                ctx.fill();
                ctx.closePath();

                ctx.restore();
            }
            //changes cursor if hovered over the end position
            if (thisViewer.dragging && Math.sqrt(Math.pow(xPos - polygonVerts[0][0], 2) + Math.pow(yPos - polygonVerts[0][1], 2)) < thisViewer.stopRadius) {
                document.body.style.cursor = 'pointer';
            } else {
                document.body.style.cursor = 'crosshair';
            }
            //otherwise draws box for box highlighting
        } else if (thisViewer.highlight === "box") {
            var dragEndX = e.offsetX;
            var dragEndY = e.offsetY;

            lastCursorPositionX = dragEndX;
            lastCursorPositionY = dragEndY;

            if (thisViewer.dragging) {
                ctx.clearRect(0, 0, overlayCanvasElement.width, overlayCanvasElement.height);
                ctx.save();
                ctx.setLineDash([10, 10]);
                ctx.strokeStyle = 'rgba(255,0,0,1)';
                ctx.lineWidth = 2;
                ctx.strokeRect(dragStartX, dragStartY, dragEndX - dragStartX, dragEndY - dragStartY);
                ctx.restore();
            }
        }

    });

    overlayCanvasElement.addEventListener('mouseleave', function(e) {
        if (thisViewer.dragging) {
            thisViewer.dragging = false;
            polygonVerts = [];
            ctx.clearRect(0, 0, overlayCanvasElement.width, overlayCanvasElement.height);
            if (thisViewer.highlight === "box") {
                var vertices = [
                    [dragStartX, dragStartY],
                    [dragStartX, lastCursorPositionY],
                    [lastCursorPositionX, lastCursorPositionY],
                    [lastCursorPositionX, dragStartY]
                ];
                thisViewer.generateDragSelection(vertices);
            }
        }
        document.body.style.cursor = 'default';
    });
}

/**
 * Clear hightlight selection
 */
embeddingViewerScatterCanvas.prototype.clearHighlight = function() {
    var canvas = document.getElementById('embedding-canvas-overlay');
    var ctx = canvas.getContext('2d');
    ctx.clearRect(0, 0, canvas.width, canvas.height);
}

/**
 * Generated the necessary html structure for this viewer
 */
embeddingViewerScatterCanvas.prototype.generateStructure = function() {
    var embeddingContainer = $("#embedding-draw");
    var extJsContainer = Ext.getCmp('embeddingExtJSWrapper');

    // Cleanup
    embeddingContainer.empty();

    embeddingContainer.append('<div id="embedding-draw-inner" style="position: relative" />');
    var embeddingContainerInner = $('#embedding-draw-inner');

    // Add a canvas
    embeddingContainerInner.append('<canvas id="embedding-canvas"></canvas>');

    // Add the overlay canvas
    embeddingContainerInner.append('<canvas id="embedding-canvas-overlay"></canvas>');

    // Add the hover cell highlight canvas
    embeddingContainerInner.append('<canvas id="embedding-canvas-hover"></canvas>');

    // Add event listeners to the top canvas
    var eventCanvas = document.getElementById('embedding-canvas-hover');
    this.setupOverlayEvents(eventCanvas);

    this.resizeElements();
}

embeddingViewerScatterCanvas.prototype.updateCanvasSize = function(width, height) {
    var embCanvasOverlay = document.getElementById('embedding-canvas-overlay');
    embCanvasOverlay.width = width;
    embCanvasOverlay.height = height;

    var embCanvas = document.getElementById('embedding-canvas');
    embCanvas.width = width;
    embCanvas.height = height;

    var embCanvasHover = document.getElementById('embedding-canvas-hover');
    embCanvasHover.width = width;
    embCanvasHover.height = height;
}

embeddingViewerScatterCanvas.prototype.resizeElements = function() {
    var extJsContainer = Ext.getCmp('embeddingExtJSWrapper');

    var plotWidth = extJsContainer.body.getWidth(true);
    var plotHeight = extJsContainer.body.getHeight(true);

    // Which dim is limiting?
    this.size = Math.min(plotWidth, plotHeight);

    this.updateCanvasSize(plotWidth, plotHeight);

    if (plotHeight < plotWidth) {
        this.hpad = (plotWidth - this.size) / 2
        this.vpad = 0;
    } else {
        this.hpad = 0;
        this.vpad = (plotHeight - this.size) / 2
    }

    // Setup the css for the containers
    $('#embedding-canvas').css({
        'position': 'absolute',
        'top': '0px',
        'left': '0px'
    });

    $('#embedding-canvas-overlay').css({
        'position': 'absolute',
        'top': '0px',
        'left': '0px'
    });

    $('#embedding-canvas-hover').css({
        'position': 'absolute',
        'top': '0px',
        'left': '0px'
    });

    $('#embedding-draw-inner').css({
        'height': plotHeight + 'px',
        'width': plotWidth + 'px',
        'top': '0px',
        'left': '0px'
    });
}

/**
 * Draws the embedding
 */
embeddingViewerScatterCanvas.prototype.draw = function(callback) {
    var dataCntr = new dataController();
    var embViewer = new embeddingViewer();
    var config = embViewer.getConfig();
    var type = config.type;
    var embeddingType = config.embeddingType;
    var thisViewer = this;

    var extJsContainer = Ext.getCmp('embeddingExtJSWrapper');
    var plotHeight = extJsContainer.body.getHeight(true);
    var plotWidth = extJsContainer.body.getWidth(true);

    // Show the wait window
    embViewer.showWait();

    //var size = this.size;
    var size = Math.min(plotHeight, plotWidth);

    // Embeddings are cached
    dataCntr.getEmbedding(type, embeddingType, function(data) {
        var plotData = data;

        // Generate the colors for the data points -- async
        thisViewer.generateFillStyles(plotData, function(plotData, fillInfo) {

            thisViewer.resizeElements();

            thisViewer.calculateDomain(plotData, type, embeddingType);


            var embCanvas = document.getElementById('embedding-canvas');
            // Make the xscale
            thisViewer.xScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
            thisViewer.xScaleRangeMax = size * thisViewer.rangeScaleFactor;
            var xScale = pagHelpers.linearScaleGenerator(thisViewer.xScaleDomainMin,
                thisViewer.xScaleDomainMax,
                thisViewer.xScaleRangeMin,
                thisViewer.xScaleRangeMax, thisViewer.hpad);

            // Make the y scale
            thisViewer.yScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
            thisViewer.yScaleRangeMax = size * thisViewer.rangeScaleFactor;
            var yScale = pagHelpers.linearScaleGenerator(thisViewer.yScaleDomainMin,
                thisViewer.yScaleDomainMax,
                thisViewer.yScaleRangeMin,
                thisViewer.yScaleRangeMax, thisViewer.vpad);

            // The size to plot at
            var pointsize = embViewer.getCurrentPointSize();

            // The border
            var stroke = embViewer.getCurrentBorder();
            var strokeColor;
            var strokeWidth;

            if (stroke === true) {
                strokeColor = embViewer.getCurrentBorderColor();
                strokeWidth = embViewer.getCurrentBorderWidth();
            }

            // Get 2d context and plot
            var ctx = embCanvas.getContext('2d');
            ctx.clearRect(0, 0, embCanvas.width, embCanvas.height);

            // Calculate 2 * pi only once
            const PI_2 = 2 * Math.PI;

            // Main plot loop
            for (var i = 0; i < plotData.length; i++) {
                var point = plotData[i];

                var xs = xScale(point[1]);
                var ys = yScale(point[2]);

                ctx.beginPath();
                ctx.arc(xs, ys, pointsize, 0, PI_2, false);
                ctx.fillStyle = point[3];
                ctx.fill();

                // Plot the border if selected
                if (stroke) {
                    ctx.lineWidth = strokeWidth;
                    ctx.strokeStyle = strokeColor;
                    ctx.stroke();
                }
            }
            embViewer.hideWait();
            
            if (typeof callback =='function') {
              callback();
            }
        }, type, embeddingType); // GenerateFillStyles
    });
}

/**
 * Generate fill styles suitable for the given dataset
 */
embeddingViewerScatterCanvas.prototype.generateFillStyles = function(plotdata, callback, type, embeddingType) {

    // Get the color config
    var ev = new embeddingViewer();
    var config = ev.getConfig();
    var colconfig = config.colors;

    if (colconfig === 'dendrogram') {
        this.generateFillStylesDendrogram(plotdata, callback);
    } else if (colconfig === 'metadata') {
        this.generateFillStylesMetadata(plotdata, callback);
    } else if (colconfig === 'geneexpression') {
        // Type and embedding type is passed here for caching the matches correctly
        this.generateFillStylesGeneExpression(plotdata, callback, type, embeddingType);
    } else if (colconfig === 'aspect') {
        this.generateFillStylesAspect(plotdata, callback, type, embeddingType);
    } else {
        this.generateDefaultFill(plotdata, callback);
    }
}

/**
 * Generate the default fill
 */
embeddingViewerScatterCanvas.prototype.generateDefaultFill = function(plotdata, callback) {
    var embV = new embeddingViewer();
    var alpha = embV.getCurrentAlpha();
    for (var i = 0; i < plotdata.length; i++) {
        plotdata[i][3] = 'rgba(0,0,0,' + alpha + ')';
    }
    callback(plotdata);
    return;
}

/**
 * Update the alpha values of an array of colors
 * @colors array of colors in #FFFFFF format
 * @newAlpha new alpha value to set in the range of 0 to 1
 * @return array of the colors with alpha set in rgba(1,1,1,1) text format
 */
embeddingViewerScatterCanvas.prototype.updateColorAlpha = function(colors, newAlpha) {
    // Prepare the palette with our alpha
    var retVals = [];
    for (var k = 0; k < colors.length; k++) {
        var color = colors[k];

        // Split the channels
        var r = parseInt(color.substring(1, 3), 16);
        var g = parseInt(color.substring(3, 5), 16)
        var b = parseInt(color.substring(5, 7), 16);

        // Add user-specified alpha
        color = 'rgba(' + r + ',' + g + ',' + b + ',' + newAlpha + ')';
        retVals[k] = color;
    }
    return retVals;

}

/**
 * Annotate the plotdata with color from the dendrogram selections and
 * call the callback with it.
 */
embeddingViewerScatterCanvas.prototype.generateFillStylesDendrogram = function(plotdata, callback) {
    var embV = new embeddingViewer();
    var selCntr = new cellSelectionController();

    // Out plot colors in rgb
    var selectedRGB = p2globalParams.dendrogram.selectionColors.selectedMainRGB;
    var selectedAltRGB = p2globalParams.dendrogram.selectionColors.selectedAltRGB;
    var deselectedRGB = p2globalParams.dendrogram.selectionColors.deselectedRGB;

    var alpha = embV.getCurrentAlpha();
    ev.setTitle(''); // not sure what to say here

    if (embV.currentConfiguration.dendrogramColorInfo.nodeType == "vertical") {
        var curPrimSel = selCntr.getSelection("currentPrimarySel");
        for (var i = 0; i < plotdata.length; i++) {
            var cellid = plotdata[i][0];
            if ($.inArray(cellid, curPrimSel) > -1) {
                plotdata[i][3] = 'rgba(' + selectedRGB[0] + ',' + selectedRGB[1] +
                    ' , ' + selectedRGB[2] + ',' + alpha + ')';
            } else {
                plotdata[i][3] = 'rgba(' + deselectedRGB[0] + ',' + deselectedRGB[1] +
                    ' , ' + deselectedRGB[2] + ',' + alpha + ')';
            }
        }
        callback(plotdata);
        return;
    } else if (embV.currentConfiguration.dendrogramColorInfo.nodeType == "horizontal") {
        var curPrimSel = selCntr.getSelection("currentPrimarySel");
        var curAltSel = selCntr.getSelection("currentAltSel");
        for (var i = 0; i < plotdata.length; i++) {
            var cellid = plotdata[i][0];
            if ($.inArray(cellid, curPrimSel) > -1) {
                plotdata[i][3] = 'rgba(' + selectedRGB[0] + ',' + selectedRGB[1] +
                    ' , ' + selectedRGB[2] + ',' + alpha + ')';
            } else if ($.inArray(cellid, curAltSel) > -1) {
                plotdata[i][3] = 'rgba(' + selectedAltRGB[0] + ',' + selectedAltRGB[1] +
                    ' , ' + selectedAltRGB[2] + ',' + alpha + ')';
            } else {
                plotdata[i][3] = 'rgba(' + deselectedRGB[0] + ',' + deselectedRGB[1] +
                    ' , ' + deselectedRGB[2] + ',' + alpha + ')';
            }
        }
        callback(plotdata);
        return;
    }
}

/**
 * Generate fill style information from metadata
 * @param plotdata the plotdata array to augment with colors
 * @callback the callback function
 */
embeddingViewerScatterCanvas.prototype.generateFillStylesMetadata = function(plotdata, callback) {
    // Get the configuration information that we need
    var ev = new embeddingViewer();
    var config = ev.getConfig();

    var dataCntr = new dataController();

    var alpha = ev.getCurrentAlpha();

    // Get the embedding viewer config and the name
    // of the metadata we want to plot
    var metadataColorInfo = config.metaDataColorInfo;
    var metadataName = metadataColorInfo.metadataName;
    ev.setTitle(metadataName);

    dataCntr.getCellMetadata(function(metadata) {
        var colorData = metadata[metadataName].data;
        var colorPalette = metadata[metadataName].palette;

	var cellsWithoutMetadata = [];
	var undefinedColor = false;

        for (var i = 0; i < plotdata.length; i++) {
            var cellId = plotdata[i][0];
            var clusterId = colorData[cellId];
            var clusterColor;

            if (typeof clusterId === 'undefined') {
		cellsWithoutMetadata.push(cellId);
                clusterColor = '#000000'; // default to black
            } else {
                clusterColor = colorPalette[clusterId];
            }

            if (typeof clusterColor == 'undefined') {
		undefinedColor = true;
		clusterColor = '#000000';
            }

            // TODO use objct function here
            // Split the channels
            var r = parseInt(clusterColor.substring(1, 3), 16);
            var g = parseInt(clusterColor.substring(3, 5), 16)
            var b = parseInt(clusterColor.substring(5, 7), 16);

            // Add user-specified alpha
            clusterColor = 'rgba(' + r + ',' + g + ',' + b + ',' + alpha + ')';

            plotdata[i][3] = clusterColor;
        } // for

	// Report any errors to the console
	if (undefinedColor) {
	    console.warn('Missing color found');
	}
	if (cellsWithoutMetadata.lenth > 0) {
	    console.warn('At least one cell has missing metadata');
	}
	

        callback(plotdata);
        return;
    });
}

/**
 * Generate fill styles by gene expression
 * @param plotdata the plotdata to augment with colors
 * @param callback the callback function
 * @param type the embedding type, used for caching the match indexes
 * @param embeddingType the embedding type, used for caching the match indexes
 */
embeddingViewerScatterCanvas.prototype.generateFillStylesGeneExpression = function(plotdata, callback, type, embeddingType) {
    var evSC = this;

    var ev = embeddingViewer();
    var config = ev.getConfig();
    var dataCntr = new dataController();
    var alpha = ev.getCurrentAlpha();

    evSC.abortPendingRequest();

    // We only end up using this for the number of cell
    // which is a complete waste, but it takes less than 1ms so not worth removing
    dataCntr.getCellOrder(function(data) {

        var cellIndexStart = 0;
        var cellIndexEnd = data.length;
        var geneSelection = [config.geneexpressionColorInfo.geneid];
	ev.setTitle(geneSelection+" gene");
        // For potential cancelling
        evSC.colorAJAXrequest = dataCntr.getExpressionValuesSparseByCellIndex(
            geneSelection, cellIndexStart, cellIndexEnd,
            function(data) {
                var heatView = new heatmapViewer();
                // Get palette
                var palSize = heatView.palManager.getNumberOfColors();
                var pal = heatView.palManager.getPaletteColors();

                // Calculate normalisation values
                // Copy-paste from heatmap plot
                // TODO: We need a better abstraction form palettes
                var rowSum = 0;
                var rowMax = data.array[0][0];
                var rowMin = 0;
                for (var j = 0; j < data.array.length; j++) {
                    rowMax = Math.max(rowMax, data.array[j][0]);
                    //rowMin = Math.min(rowMin, data.array[j][0]);
                    rowSum += data.array[j][0];
                }
                rowMax = Math.max(rowMax, 1e-2);

                var rowMean = rowSum / data.array.length;
                var maxAbsValue = Math.max(Math.abs(rowMin - rowMean), Math.abs(rowMax - rowMean));

                // Update the palette colors with our alpha
                var palAlpha = evSC.updateColorAlpha(pal, alpha);

                // colorMapper is a function
                // use it for consistency with heatmap
                var colorMapper = heatView.palManager.getMeanClampedColorMapper(rowMean, maxAbsValue, palSize);
                var cacheId = type + '_' + embeddingType;

                // Check if the cache exists and build it if necessary
                if (typeof evSC.fillStylesGeneExpressionOrder[cacheId] === 'undefined') {
                    evSC.fillStylesGeneExpressionOrder[cacheId] = [];
                    evSC.doMatching(plotdata, undefined, evSC, callback, data, cacheId, colorMapper, palAlpha);
                } else {
                    var cache = evSC.fillStylesGeneExpressionOrder[cacheId];
                    evSC.doMatchingFromCache(plotdata, undefined, callback, data, cache, colorMapper, palAlpha);
                }

                return;
            }); // getExpressionValuesSparseByCellIndex
    }); // getCellOrder
} // generateFillStylesGeneExpression

embeddingViewerScatterCanvas.prototype.doMatching = function(plotdata, i, evSC, callback, data, cacheId, colorMapper, palAlpha) {
    if (i == undefined) {
        i = 0;
    }
    if (i < plotdata.length) {
        // The size of the batch is a trade off between slowing the
        // processing and maintaining responsiveness
        var batchSize = 500;
        for (var j = i; j < i + batchSize && j < plotdata.length; j++) {
            var cellId = plotdata[j][0];
            var index = data.rownames.indexOf(cellId);

            evSC.fillStylesGeneExpressionOrder[cacheId][j] = index;

            var color;
            if (index < 0) {
                console.warn('Embedding plotter found a cell that does not have and entry in the expression matrix. Cellid: "' +
                    cellId + '". This is an error with the data provided by server.');

                color = '#000000'; // default to black
            } else {
                var palIndex = colorMapper(data.array[index][0]);
                color = palAlpha[palIndex];
            }
            plotdata[j][3] = color;
        } // for

        if (j == plotdata.length) {
            callback(plotdata);
        }

        var nextBatch = function() {
            embeddingViewerScatterCanvas.prototype.doMatching(plotdata, i + batchSize, evSC, callback, data, cacheId, colorMapper, palAlpha);
        };

        // Process the next batch immediately,
        // Allowing other pending event to run in the meanwhile
        setTimeout(nextBatch, 0);
    }
}

embeddingViewerScatterCanvas.prototype.doMatchingFromCache = function(plotdata, i, callback, data, cache, colorMapper, palAlpha) {
    var batchSize = 50000;
    if (i == undefined) {
        i = 0;
    }

    if (i < plotdata.length) {
        for (var j = i; j < i + batchSize && j < plotdata.length; j++) {
            // Retrieve from cache
            var index = cache[j];
            if (index < 0) {
                plotdata[j][3] = '#000000'; // default to black
            } else {
                var palIndex = colorMapper(data.array[index][0]);
                plotdata[j][3] = palAlpha[palIndex];
            }
        }

        if (j == plotdata.length) {
            callback(plotdata);
        }

        var nextBatch = function() {
            embeddingViewerScatterCanvas.prototype.doMatchingFromCache(plotdata, i + batchSize, callback, data, cache, colorMapper, palAlpha);
        }

        setTimeout(nextBatch, 0);
    }
}

/**
 * Aborts a pernding AJAX request
 */
embeddingViewerScatterCanvas.prototype.abortPendingRequest = function() {
    var evSC = this;
    // Check if there is a request running already
    if ((typeof evSC.colorAJAXrequest !== 'undefined') && evSC.colorAJAXrequest !== null) {
        if (evSC.colorAJAXrequest.readyState != 4) { //Not DONE
            // A previous request exists and is still running
            // cancel it
            evSC.colorAJAXrequest.abort();
        }
    }
}

/**
 * Generate fill styles by aspect
 * @param plotdata the plotdata to augment with colors
 * @param callback the callback function
 * @param type the embedding type, used for caching the match indexes
 * @param embeddingType the embedding type, used for caching the match indexes
 */
embeddingViewerScatterCanvas.prototype.generateFillStylesAspect = function(plotdata, callback, type, embeddingType) {
    var evSC = this;
    var ev = new embeddingViewer();
    var config = ev.getConfig();
    var dataCntr = new dataController();
    var alpha = ev.getCurrentAlpha();
    var aspectId = [ev.getAspectColorInfo().aspectid];
    ev.setTitle(aspectId);
    evSC.abortPendingRequest();
    dataCntr.getCellOrder(function(data) {
        var cellIndexStart = 0;
        var cellIndexEnd = data.length;

        evSC.colorAJAXrequest = dataCntr.getAspectMatrixByAspect(cellIndexStart, cellIndexEnd, aspectId, function(data) {

            var heatView = new aspectHeatmapViewer();
            // Get palette
            var palSize = heatView.palManager.getNumberOfColors();
            var pal = heatView.palManager.getPaletteColors();

            // Calculate normalisation values
            // Copy-paste from heatmap plot
            // TODO: We need a better abstraction form palettes
            var rowSum = 0;
            var rowMax = data.array[0][0];
            var rowMin = data.array[0][0];
            for (var j = 0; j < data.array.length; j++) {
                rowMax = Math.max(rowMax, data.array[j][0]);
                rowMin = Math.min(rowMin, data.array[j][0]);
                rowSum += data.array[j][0];
            }

            var rowMean = rowSum / data.array.length;
            var maxAbsValue = Math.max(Math.abs(rowMin - rowMean), Math.abs(rowMax - rowMean));

            // Update the palette colors with our alpha
            var palAlpha = evSC.updateColorAlpha(pal, alpha);

            // colorMapper is a function
            // use it for consistency with heatmap
            var colorMapper = heatView.palManager.getMeanClampedColorMapper(rowMean, maxAbsValue, palSize);

            var cacheId = type + '_' + embeddingType;

            if (typeof evSC.fillStylesGeneExpressionOrder[cacheId] === 'undefined') {
                evSC.fillStylesGeneExpressionOrder[cacheId] = [];
                evSC.doMatching(plotdata, undefined, evSC, callback, data, cacheId, colorMapper, palAlpha);
            } else {
                var cache = evSC.fillStylesGeneExpressionOrder[cacheId];
                evSC.doMatchingFromCache(plotdata, undefined, callback, data, cache, colorMapper, palAlpha);
            } // if ... else

        }); //dataCntr.getAspectMatrixByAspect
    }); // dataCntr.getCellOrder
}; //generateFillStylesAspect

embeddingViewerScatterCanvas.prototype.buildCellPositionCacheByID = function(type, embeddingType, callback) {
    var thisViewer = this;
    var cacheId = type + '_' + embeddingType;

    var dataCntr = new dataController();
    dataCntr.getEmbedding(type, embeddingType, function(plotData) {

        thisViewer.cellPositionCacheByID[cacheId] = {};

        for (var i = 0; i < plotData.length; i++) {
            var point = plotData[i];
            thisViewer.cellPositionCacheByID[cacheId][point[0]] = point;
        }

        callback();
    });
}

embeddingViewerScatterCanvas.prototype.buildCellPositionCacheByIndex = function(type, embeddingType, callback) {

    var thisViewer = this;
    thisViewer.indexCacheBuilding = true;
    var cacheId = type + '_' + embeddingType;

    var dataCntr = new dataController();
    dataCntr.getEmbedding(type, embeddingType, function(plotData) {
        dataCntr.getCellOrderHash(function(cellorderData) {
            thisViewer.cellPositionCacheByIndex[cacheId] = {};

            for (var i = 0; i < plotData.length; i++) {
                var point = plotData[i];
                thisViewer.cellPositionCacheByIndex[cacheId][cellorderData[point[0]]] = point;
            }
            thisViewer.indexCacheBuilding = false;
            callback();
        });
    });
}

embeddingViewerScatterCanvas.prototype.getCellPositionFromCacheByIndex = function(type, embeddingType, cellindex, callback) {
    var thisViewer = this;
    var cacheId = type + '_' + embeddingType;

    if (!(this.indexCacheBuilding)) {
      if (typeof thisViewer.cellPositionCacheByIndex[cacheId] !== 'undefined') {
          callback(thisViewer.cellPositionCacheByIndex[cacheId][cellindex]);
      } else {
          thisViewer.buildCellPositionCacheByIndex(type, embeddingType, function() {
              callback(thisViewer.cellPositionCacheByIndex[cacheId][cellindex]);
          });
      }
    }
}

/**
 * Get the cell position from cache building the index if required
 */
embeddingViewerScatterCanvas.prototype.getCellPositionFromCacheByID = function(type, embeddingType, cellid, callback) {
    var thisViewer = this;
    var cacheId = type + '_' + embeddingType;
    if (typeof thisViewer.cellPositionCacheByID[cacheId] !== 'undefined') {
        callback(thisViewer.cellPositionCacheByID[cacheId][cellid]);
    } else {
        thisViewer.buildCellPositionCacheByID(type, embeddingType, function() {
            callback(thisViewer.cellPositionCacheByID[cacheId][cellid]);
        });
    }
}


/**
 * Remove the hover cell hightlight
 */
embeddingViewerScatterCanvas.prototype.clearHighlightCell = function() {
    var canvas = document.getElementById('embedding-canvas-hover');
    var ctx = canvas.getContext('2d');
    ctx.clearRect(0, 0, canvas.width, canvas.height);
}

/**
 * Highlight the position of a specific cell
 */
embeddingViewerScatterCanvas.prototype.highlightCellByID = function(cellid) {
    var embViewer = new embeddingViewer();
    var thisViewer = this;

    var config = embViewer.getConfig();
    var dataCntr = new dataController();
    var type = config.type;
    var embeddingType = config.embeddingType;

    var size = this.size;

    this.getCellPositionFromCacheByID(type, embeddingType, cellid, function(point) {
        thisViewer.calculateDomain(null, type, embeddingType);
        thisViewer.xScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
        thisViewer.xScaleRangeMax = size * thisViewer.rangeScaleFactor;
        var xScale = pagHelpers.linearScaleGenerator(thisViewer.xScaleDomainMin,
            thisViewer.xScaleDomainMax,
            thisViewer.xScaleRangeMin,
            thisViewer.xScaleRangeMax, thisViewer.hpad);

        thisViewer.yScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
        thisViewer.yScaleRangeMax = size * thisViewer.rangeScaleFactor;
        var yScale = pagHelpers.linearScaleGenerator(thisViewer.yScaleDomainMin,
            thisViewer.yScaleDomainMax,
            thisViewer.yScaleRangeMin,
            thisViewer.yScaleRangeMax, thisViewer.vpad);

        var pointsize = embViewer.getCurrentPointSize();

        var canvas = document.getElementById('embedding-canvas-hover');
        var ctx = canvas.getContext('2d');

        ctx.save();
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        ctx.lineWidth = 4;
        ctx.fillStyle = '#FF2222';

        var xs = xScale(point[1]);
        var ys = yScale(point[2]);

        ctx.strokeStyle = 'black';
        ctx.beginPath();
        ctx.arc(xs, ys, pointsize, 0, 2 * Math.PI, false);
        ctx.stroke();
        ctx.fill();

        ctx.restore();
    });
}

/**
 * Highlight the position of a specific cell given its index in cell order
 */
embeddingViewerScatterCanvas.prototype.highlightCellByIndex = function(cellindex) {
    var embViewer = new embeddingViewer();
    var thisViewer = this;

    var config = embViewer.getConfig();
    var dataCntr = new dataController();
    var type = config.type;
    var embeddingType = config.embeddingType;

    var size = this.size;

    this.getCellPositionFromCacheByIndex(type, embeddingType, cellindex, function(point) {
        thisViewer.calculateDomain(null, type, embeddingType);
        thisViewer.xScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
        thisViewer.xScaleRangeMax = size * thisViewer.rangeScaleFactor;
        var xScale = pagHelpers.linearScaleGenerator(thisViewer.xScaleDomainMin,
            thisViewer.xScaleDomainMax,
            thisViewer.xScaleRangeMin,
            thisViewer.xScaleRangeMax, thisViewer.hpad);

        thisViewer.yScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
        thisViewer.yScaleRangeMax = size * thisViewer.rangeScaleFactor;
        var yScale = pagHelpers.linearScaleGenerator(thisViewer.yScaleDomainMin,
            thisViewer.yScaleDomainMax,
            thisViewer.yScaleRangeMin,
            thisViewer.yScaleRangeMax, thisViewer.vpad);

        var pointsize = embViewer.getCurrentPointSize();

        var canvas = document.getElementById('embedding-canvas-hover');
        var ctx = canvas.getContext('2d');

        ctx.save();
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        ctx.lineWidth = 4;
        ctx.fillStyle = '#FF2222';

        var xs = xScale(point[1]);
        var ys = yScale(point[2]);

        ctx.strokeStyle = 'black';
        ctx.beginPath();
        ctx.arc(xs, ys, pointsize, 0, 2 * Math.PI, false);
        ctx.stroke();
        ctx.fill();

        ctx.restore();
    });
}
