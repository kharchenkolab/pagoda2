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
    console.log('Initializing scatterEmbeddingCanvas...');

    // Here we need to tap into the extjs container
    // component resize function 
    var element = Ext.getCmp('embedding-app-container');
    element.onResize = function(){
	embView = new embeddingViewer();
	embView.redraw();
    };

    var evtBus = new eventBus();
    // Dendrogram node has been clicked
    // Set the coloring scheme to dendrogram ( which uses the selection controller )
    // and call updateColors
    evtBus.register("dendrogram-node-click", null, function(e,p) {
	var embV = new embeddingViewer();

	// Update  configuration of embedding
	// The actual selected node is passed by the cell selection
	embV.setColorConfiguration('dendrogram');
	embV.setDendrogramColorInfo({nodeType: p.type});

	embV.updateColors();
    }); 

    // This variable hold the last ajax request for expression values
    // The purpose of this is to be able to cancel this request
    // If the user makes a new request while the current one is running
    this.expressionValuesAJAXrequest;

    // We use this to cache the cell order as calculated by getExpression
    // and avoid recalculating every time. We need to cache this for 
    // each embedding seperately
    this.fillStylesGeneExpressionOrder = {};

    // Used to slightly shrink the plotting area to prevent
    // plotting on the edges
    this.rangeScaleFactor = 0.95;

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

    // Generates the html structure required for the viewer
    this.generateStructure();    
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
    function(dragStartX, dragStartY, dragEndX, dragEndY) {
	if (dragStartX > dragEndX) {
	    var t = dragEndX;
	    dragEndX =  dragStartX;
	    dragStartX = t;
	}
	if (dragStartY > dragEndY) {
	    var t = dragEndY;
	    dragEndY = dragStartY;
	    dragStartY = t;
	}
	

	thisViewer = this;
	var embViewer = new embeddingViewer();
	config = embViewer.getConfig();

	// Reversed scales
	var xScaleRev = pagHelpers.linearScaleGenerator(
	    thisViewer.xScaleRangeMin,
	    thisViewer.xScaleRangeMax,
	    thisViewer.xScaleDomainMin,
	    thisViewer.xScaleDomainMax);

	var yScaleRev = pagHelpers.linearScaleGenerator(
	    thisViewer.yScaleRangeMin,
	    thisViewer.yScaleRangeMax,
	    thisViewer.yScaleDomainMin,
	    thisViewer.yScaleDomainMax);

	var x1 = xScaleRev(dragStartX);
	var y1 = yScaleRev(dragStartY);
	var x2 = xScaleRev(dragEndX);
	var y2 = yScaleRev(dragEndY);

	var dataCntr = new dataController();
	var type = config.type;
	var embeddingType = config.embeddingType;

	dataCntr.getEmbedding(type, embeddingType, function(data) {
	    plotData = pagHelpers.jsonSerialisedToArrayOfArrays(data);
	    
	    
	    // Make the xscale
	    thisViewer.xScaleDomainMin = +Infinity;
	    thisViewer.xScaleDomainMax = -Infinity;
	    for (j = 0; j < plotData.length; j++) {
		thisViewer.xScaleDomainMin = Math.min(thisViewer.xScaleDomainMin, plotData[j][1]);
		thisViewer.xScaleDomainMax = Math.max(thisViewer.xScaleDomainMax, plotData[j][1]);
	    }
	    thisViewer.xScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
	    thisViewer.xScaleRangeMax = size * thisViewer.rangeScaleFactor;
	    var xScale = pagHelpers.linearScaleGenerator(thisViewer.xScaleDomainMin, 
							 thisViewer.xScaleDomainMax, 
							 thisViewer.xScaleRangeMin, 
							 thisViewer.xScaleRangeMax);

	    // Make the y scale
	    thisViewer.yScaleDomainMin = +Infinity;
	    thisViewer.yScaleDomainMax = -Infinity;
	    for (j = 0; j < plotData.length; j++) {
		thisViewer.yScaleDomainMin = Math.min(thisViewer.yScaleDomainMin, plotData[j][2]);
		thisViewer.yScaleDomainMax = Math.max(thisViewer.yScaleDomainMax, plotData[j][2]);
	    }
	    thisViewer.yScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
	    thisViewer.yScaleRangeMax = size * thisViewer.rangeScaleFactor;
	    var yScale = pagHelpers.linearScaleGenerator(thisViewer.yScaleDomainMin,
							 thisViewer.yScaleDomainMax, 
							 thisViewer.yScaleRangeMin,
							 thisViewer.yScaleRangeMax);

	    var pointsize = embViewer.getCurrentPointSize();

	    var ctx = document.getElementById('embedding-canvas-overlay').getContext('2d');
	    ctx.clearRect(0,0,5000,5000);

	    var cellsForSelection = new Array();
	    ctx.strokeStyle = 'red';

	    for (var i = 0; i < plotData.length; i++) {
		var point = plotData[i];
		
		if ( point[1] > x1 && point[1] < x2 && point[2] > y1 && point[2] < y2) {
		    // Point in selection
		    cellsForSelection.push(point[0]);
		    
		    var xs = xScale(point[1]);
		    var ys = yScale(point[2]);
		    
		    ctx.beginPath();
		    ctx.arc(xs, ys, pointsize, 0, 2 * Math.PI, false);
		    ctx.stroke();
		}
	    } // for

	    var cellSelCntr = new cellSelectionController();
	    cellSelCntr.setSelection('embSelection', cellsForSelection, 'Embedding Selection', new Object());

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

embeddingViewerScatterCanvas.prototype.setupOverlayEvents = function(overlayCanvasElement) {
    var thisViewer = this;

    var dragging = false;
    var dragStartX = 0;
    var dragStartY = 0;
    var ctx = overlayCanvasElement.getContext('2d');

    overlayCanvasElement.addEventListener('mouseover', function(e) {
	document.body.style.cursor = 'crosshair';
    });

    overlayCanvasElement.addEventListener('mousedown', function(e) {
	dragStartX = e.layerX;
	dragStartY = e.layerY;
	dragging = true;
    });

    overlayCanvasElement.addEventListener('mouseup', function(e) {
	if (dragging) {
	    // Dragging complete
	    var dragEndX = e.layerX;
	    var dragEndY = e.layerY;

	    

	    thisViewer.generateDragSelection(dragStartX, dragStartY, dragEndX, dragEndY);
	}
	dragging = false;
    });
    
    overlayCanvasElement.addEventListener('mousemove', function(e) {
	var dragEndX =  e.layerX;
	var dragEndY = e.layerY;
	

	if (dragging) {
	    // TODO: clear the correct coordinates
	    ctx.clearRect(0,0,4000,4000);
	    ctx.save();
	    ctx.setLineDash([10,10]);
	    ctx.strokeStyle = '#FF0000';
	    ctx.lineWidth = 2;
	    ctx.strokeRect(dragStartX, dragStartY, dragEndX - dragStartX, dragEndY - dragStartY);
	    ctx.restore();
	}

    });

    overlayCanvasElement.addEventListener('mouseleave', function(e) {

	if (dragging) {
	    ctx.clearRect(0,0,4000,4000);
	}
	dragging = false;
	document.body.style.cursor = 'default';
    });

}

/**
 * Generated the necessary html structure for this viewer
 */
embeddingViewerScatterCanvas.prototype.generateStructure = function() {
    var embeddingContainer = $("#embedding-draw"); 
    var extJsContainer = Ext.getCmp('embeddingExtJSWrapper');

    // Clears the loader and anything else left in the container
    // From other embeddings
    embeddingContainer.empty();

    embeddingContainer.append('<div id="embedding-draw-inner" style="position: relative" />');
    embeddingContainerInner = $('#embedding-draw-inner');

    // Add a canvas
    embeddingContainerInner.append('<canvas id="embedding-canvas"></canvas>');

    // Add the overlay canvas
    embeddingContainerInner.append('<canvas id="embedding-canvas-overlay"></canvas>');

    var embCanvasOverlay = document.getElementById('embedding-canvas-overlay');
    this.setupOverlayEvents(embCanvasOverlay);

    this.resizeElements();
}


embeddingViewerScatterCanvas.prototype.resizeElements = function() {
    var extJsContainer = Ext.getCmp('embeddingExtJSWrapper');

    var plotHeight = extJsContainer.body.getHeight(true);
    var plotWidth = extJsContainer.body.getWidth(true);

    // Which dim is limiting?
    size = Math.min(plotWidth, plotHeight);


    var embCanvasOverlay = document.getElementById('embedding-canvas-overlay');
    embCanvasOverlay.width = size;
    embCanvasOverlay.height = size;

    var embCanvas = document.getElementById('embedding-canvas');
    embCanvas.width = size;
    embCanvas.height = size;


    // Center it in x and y
    var vpad = 0;
    var hpad = 0;

    if (plotHeight < plotWidth) {
	hpad = (plotWidth - size)/2
    } else {
	vpad = (plotHeight - size) /2
    }

    // Setup the css for the containers
    $('#embedding-canvas').css({
	'margin-top': vpad,
	'margin-left': hpad,
	'position': 'absolute',
	'top': '0px',
	'left': '0px'
    });

    $('#embedding-canvas-overlay').css({
	'margin-top': vpad,
	'margin-left': hpad,
	'position': 'absolute',
	'top': 0,
	'left': 0
    });

    $('#embedding-draw-inner').css({
	'height': plotHeight + 'px',
	'width': plotWidth + 'px'
    });

}

/**
 * Draws the embedding
 */
embeddingViewerScatterCanvas.prototype.draw = function() {
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

    // Embeddings are cached
    dataCntr.getEmbedding(type, embeddingType, function(data) {
	plotData = pagHelpers.jsonSerialisedToArrayOfArrays(data);

	// Generate the colors for the data points -- async
	thisViewer.generateFillStyles(plotData, function(plotData, fillInfo) {
	    
	thisViewer.resizeElements();

	    var embCanvas = document.getElementById('embedding-canvas');
	    // Make the xscale
	    thisViewer.xScaleDomainMin = +Infinity;
	    thisViewer.xScaleDomainMax = -Infinity;
	    for (j = 0; j < plotData.length; j++) {
		thisViewer.xScaleDomainMin = Math.min(thisViewer.xScaleDomainMin, plotData[j][1]);
		thisViewer.xScaleDomainMax = Math.max(thisViewer.xScaleDomainMax, plotData[j][1]);
	    }
	    thisViewer.xScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
	    thisViewer.xScaleRangeMax = size * thisViewer.rangeScaleFactor;
	    var xScale = pagHelpers.linearScaleGenerator(thisViewer.xScaleDomainMin, 
							 thisViewer.xScaleDomainMax, 
							 thisViewer.xScaleRangeMin, 
							 thisViewer.xScaleRangeMax);

	    // Make the y scale
	    thisViewer.yScaleDomainMin = +Infinity;
	    thisViewer.yScaleDomainMax = -Infinity;
	    for (j = 0; j < plotData.length; j++) {
		thisViewer.yScaleDomainMin = Math.min(thisViewer.yScaleDomainMin, plotData[j][2]);
		thisViewer.yScaleDomainMax = Math.max(thisViewer.yScaleDomainMax, plotData[j][2]);
	    }
	    thisViewer.yScaleRangeMin = (size * (1 - thisViewer.rangeScaleFactor));
	    thisViewer.yScaleRangeMax = size * thisViewer.rangeScaleFactor;
	    var yScale = pagHelpers.linearScaleGenerator(thisViewer.yScaleDomainMin,
							 thisViewer.yScaleDomainMax, 
							 thisViewer.yScaleRangeMin,
							 thisViewer.yScaleRangeMax);

	    // The size to plot at
	    var pointsize = embViewer.getCurrentPointSize();
	    
	    // The border
	    var stroke;
	    var strokeColor;
	    var strokeWidth = 1;

	    var stroke = embViewer.getCurrentBorder();

	    if (stroke === true ) {
		strokeColor = embViewer.getCurrentBorderColor();
		strokeWidth = embViewer.getCurrentBorderWidth();
	    }

	    // Get 2d context and plot
	    var ctx = embCanvas.getContext('2d');
	    ctx.clearRect(0,0,5000,5000);
	    t0 = performance.now()
	    // Main plot loop
	    for (var i = 0; i < plotData.length; i++) {
		var point = plotData[i];

		var xs = xScale(point[1]);
		var ys = yScale(point[2]);

		// TODO: Get the correct color for this point
		// var alpha
		ctx.beginPath();
		ctx.arc(xs, ys, pointsize, 0, 2 * Math.PI, false);
		ctx.fillStyle = point[3];
		ctx.fill();

		// Plot the border
		if (stroke) {
		    ctx.lineWidth = strokeWidth;
		    ctx.strokeStyle = strokeColor;
		    ctx.stroke();
		}
	    }

	    embViewer.hideWait();


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
    } else if (colconfig === 'geneexpression')  {
	// Type and embedding type is passed here for caching the matches correctly
	this.generateFillStylesGeneExpression(plotdata, callback, type, embeddingType);
    } else {
	this.generateDefaultFill(plotdata,callback);
    }
}

/**
 * Generate the default fill
 */
embeddingViewerScatterCanvas.prototype.generateDefaultFill = function(plotdata, callback) {
    var embV = new embeddingViewer();
    var alpha = embV.getCurrentAlpha();
    for (var i = 0; i < plotdata.length; i++) {
	plotdata[i][3] = 'rgba(0,0,0,'  + alpha  + ')';
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
	color = colors[k];
	
	// Split the channels
	var r = parseInt(color.substring(1,3),16);
	var g = parseInt(color.substring(3,5),16)
	var b = parseInt(color.substring(5,7),16);

	// Add user-specified alpha
	color = 'rgba('+ r+','+g+','+b+','+newAlpha+')';
	retVals[k] = color;
    }
    return retVals;

}

/** 
 * Annotate the plotdata with color from the dendrogram selections and 
 * call the callback with it.
 */
embeddingViewerScatterCanvas.prototype.generateFillStylesDendrogram = function (plotdata, callback) {
    var embV = new embeddingViewer();
    var selCntr =  new cellSelectionController();

    // Out plot colors in rgb
    var selectedRGB = p2globalParams.dendrogram.selectionColors.selectedMainRGB;
    var selectedAltRGB =  p2globalParams.dendrogram.selectionColors.selectedAltRGB;
    var deselectedRGB =  p2globalParams.dendrogram.selectionColors.deselectedRGB;
    
    var alpha = embV.getCurrentAlpha();

    if (embV.currentConfiguration.dendrogramColorInfo.nodeType == "vertical") {
	var curPrimSel = selCntr.getSelection("currentPrimarySel");
	for (var i = 0; i < plotdata.length; i++) {
	    var cellid = plotdata[i][0];
	    if ($.inArray(cellid, curPrimSel) > -1) { 
		plotdata[i][3] = 'rgba(' +  selectedRGB[0] + ',' +  selectedRGB[1] + 
		    ' , ' + selectedRGB[2] +  ','  + alpha  + ')';
	    } else {
		plotdata[i][3] =  'rgba(' +  deselectedRGB[0] + ',' +  deselectedRGB[1] + 
		    ' , ' + deselectedRGB[2] +  ','  + alpha  + ')';
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
		plotdata[i][3] = 'rgba(' +  selectedRGB[0] + ',' +  selectedRGB[1] + 
		    ' , ' + selectedRGB[2] +  ','  + alpha  + ')';
	    } else if ($.inArray(cellid, curAltSel) > -1) {
		plotdata[i][3] = 'rgba(' +  selectedAltRGB[0] + ',' +  selectedAltRGB[1] + 
		    ' , ' + selectedAltRGB[2] +  ','  + alpha  + ')';
	    } else {
		plotdata[i][3] =  'rgba(' +  deselectedRGB[0] + ',' +  deselectedRGB[1] + 
		    ' , ' + deselectedRGB[2] +  ','  + alpha  + ')';
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

    dataCntr.getCellMetadata(function(metadata) {
	colorData = metadata[metadataName].data;
	colorPalette = metadata[metadataName].palette;

	for (var i = 0; i < plotdata.length; i++) {
	    var cellId = plotdata[i][0];
	    var clusterId = colorData[cellId];
	    var clusterColor;
	    
	    if (typeof clusterId === 'undefined') {
		// There is a cell that is not in the metadata
		console.warn('Embedding plotter found a cell that does not have and entry ' +
			     'in the selected metadata. Cellid: "' + cellId + '" metadata: "' +
			     metadataName + '". This is an error with the data provided by server.');
		clusterColor = '#000000'; // default to black
	    } else {
		clusterColor = colorPalette[clusterId-1];
	    }

	    if (typeof clusterColor == 'undefined') {
		debugger;
	    }

	    // TODO use objct function here
	    // Split the channels
	    var r = parseInt(clusterColor.substring(1,3),16);
	    var g = parseInt(clusterColor.substring(3,5),16)
	    var b = parseInt(clusterColor.substring(5,7),16);

	    // Add user-specified alpha
	    clusterColor = 'rgba('+ r+','+g+','+b+','+alpha+')';

	    plotdata[i][3] = clusterColor;
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

    // Check if there is a request running already
    if (typeof evSC.expressionValuesAJAXrequest !== 'undefined') {

	if (evSC.expressionValuesAJAXrequest.readyState != 4) {  //Not DONE
	    // A previous request exists and is still running
	    // cancel it
	    evSC.expressionValuesAJAXrequest.abort();
	}
    }
    
    // We only end up using this for the number of cell 
    // which is a complete waste, but it takes less than 1ms so not worth removing
    dataCntr.getCellOrder (function(data) {
	var cellIndexStart = 0;
	var cellIndexEnd = data.length;
	var geneSelection = [config.geneexpressionColorInfo.geneid];

	evSC.expressionValuesAJAXrequest = 
	    dataCntr.getExpressionValuesSparseByCellIndex(
		geneSelection, cellIndexStart, cellIndexEnd, function(data) {

		    var heatView = new heatmapViewer();
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

		    if ( typeof evSC.fillStylesGeneExpressionOrder[cacheId] === 'undefined') {

			// We don't have the cache for the order matching for this embedding saved
			// So we need to calculate it and cache it on the way
			evSC.fillStylesGeneExpressionOrder[cacheId] = [];

			// NOTE: The following construction ensures that 
			// The interfact doesn't lock up during the long 
			// calculation time of the color matching
			function doMatching(plotdata, i) {
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

				    // Cache
//				    evSC.fillStylesGeneExpressionOrder[cacheId][cellId] = index;
				    evSC.fillStylesGeneExpressionOrder[cacheId][j] = index;

		    		    var color;
		    		    if (index < 0) {
		    			console.warn('Embedding plotter found a cell that does not' +
						     'have and entry in the expression matrix. Cellid: "' +
						     cellId + 
		    				     '. This is an error with the data provided by server.');
		    			color = '#000000'; // default to black
		    		    } else {
		    			var palIndex = colorMapper(data.array[index][0]);
		    			color = palAlpha[palIndex];
		    		    }
		    		    plotdata[j][3] = color;
		    		}

				if (j == plotdata.length) {
				    callback(plotdata);
				}

		    		var nextBatch = function()
				{ 
				    // Here we can optionaly update the
				    // inteface with a progress bar
				    doMatching(plotdata, i + batchSize);    
				}
				// Process the next batch immediately,
				// Allowing other pending event to run in the meanwhile
		    		setTimeout(nextBatch, 0);
		    	    }
			}
			doMatching(plotdata);
		    } else {
			// The data were cached

			var cache = evSC.fillStylesGeneExpressionOrder[cacheId];

			// Use much larger batches here
		    	var batchSize = 50000;

			// NOTE: The following construction ensures that 
			// The interfact doesn't lock up during the long 
			// calculation time of the color matching
			function doMatching(plotdata, i) {
		    	    if (i == undefined) {
		    		i = 0;
		    	    }
		    	    if (i < plotdata.length) {
				// The size of the batch is a trade off between slowing the 
				// processing and maintaining responsiveness

		    		for (var j = i; j < i + batchSize && j < plotdata.length; j++) {
				    // Retrieve from cache
//		    		    var index = cache[plotdata[j][0]];
				    index = cache[j];
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

		    		var nextBatch = function()
				{ 
				    // Here we can optionaly update the
				    // inteface with a progress bar
				    doMatching(plotdata, i + batchSize);    
				}
				// Process the next batch immediately,
				// Allowing other pending event to run in the meanwhile
		    		setTimeout(nextBatch, 0);
		    	    }
			}
			doMatching(plotdata);

		    }
		    
		    // This is the same code as above without the fix for
		    // maintaining a responsive ui
		    // // Plot
		    // // TODO: This loop is slow (2.5s for 30k cells) 
		    // // because of the lookups
		    // for (var i = 0; i < plotdata.length;  i++) {
		    // 	var cellId = plotdata[i][0];
		    // 	var index = data.rownames.indexOf(cellId);
		    // 	var color;
		    // 	if (index < 0) {
		    // 	    console.warn('Embedding plotter found a cell that does not have and entry ' +
		    // 			 'in the expression matrix. Cellid: "' + cellId + 
		    // 			 '. This is an error with the data provided by server.');
		    // 	    color = '#000000'; // default to black
		    // 	} else {
		    // 	    //var value = data.array[index][0];
		    // 	    //var plotValue = (value - rowMean) / (maxAbsValue * 2) + 0.5;
		    // 	    //var palIndex = Math.floor(plotValue * (palSize)) - 1;
		    // 	    var palIndex = colorMapper(data.array[index][0]);
		    // 	    color = palAlpha[palIndex];
		    // 	}
		    // 	plotdata[i][3] = color;
		    // }

		    //		    callback(plotdata);
		    return;
		});
    });
}

