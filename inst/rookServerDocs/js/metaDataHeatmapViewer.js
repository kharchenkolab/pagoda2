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

    metaDataHeatmapViewer.instance = this;
}

/**
 * Initialise the metadata heatmap viewer
 * @description this is separate from the constructor
 * because it's called by the container object when things
 * are ready
 */
metaDataHeatmapViewer.prototype.initialize = function () {
    console.log('Initializing Metadata viewer...');

    var metadataContainer = $('#metadata-area-container');
    metadataContainer.append('<div id="metadata-area-container-inner"></div>');

    // We need an inner container for establishing a new
    // reference for the absolute positioning of the two canvases
    var metadataContainerInner = $('#metadata-area-container-inner');
    metadataContainerInner.css({position: 'relative'});

    metadataContainerInner.append(
	'<canvas id="metadata-area"></canvas>' +
	'<canvas id="metadata-area-overlay"></canvas>'
    );

    var metadataArea = $('#metadata-area');
    metadataArea.css({
	position: 'absolute',
	top: 0,
	left: 0
    });

    var metadataAreaOverlay = $('#metadata-area-overlay');
    metadataAreaOverlay.css({
	position:'absolute',
	top: 0,
	left: 0
    });

    // Get a new clickable regions object for this heatmap
    this.clickRegions = new clickableRegions();

    // Setup the click listener
    // NOTE: At this point we are only interested in resoving
    // which line we are clicking on. In the future some of the
    // metadata should support clicking on individual cells and/or
    // regions of contigious properties.
    // For clicking on contigious chucks (e.g. clusters) we
    // will need a propery that says the the data is guaranteed to be
    // contigious with the default ordering
    (metadataAreaOverlay[0]).addEventListener('click', function(e) {
	var x = e.layerX;
	var y = e.layerY;

	var mdhv = new metaDataHeatmapViewer();
	mdhv.clickRegions.resolveClick(x,y, function(params) {
	    var embV = new embeddingViewer();
	    embV.setColorConfiguration('metadata');
	    // Here  we are just passing the params from the
	    // click region registration, we might want to change this
	    // later, but in any case keep processin in here to the minimum
	    embV.setMetadataColorInfo(params);
	    embV.updateColors();
	});
    });

    (metadataAreaOverlay[0]).addEventListener('mousemove', function(e) {
	var x = e.layerX;
	var metaV = new metaDataHeatmapViewer();
	metaV.showOverlay(x);
	var heatV = new heatmapViewer();
	heatV.showOverlay(x);
	var aspeV = new aspectHeatmapViewer();
	aspeV.showOverlay(x);
    });

    (metadataAreaOverlay[0]).addEventListener('mouseout', function(e) {
	var metaV = new metaDataHeatmapViewer();
	var heatV = new heatmapViewer();

	metaV.clearOverlay();
	heatV.clearOverlay();
	var aspeV = new aspectHeatmapViewer();
	aspeV.clearOverlay()
    });

    // Pointer change to cross hairs when over the heatmap
     (metadataAreaOverlay[0]).addEventListener('mouseenter', function(e) {
	document.body.style.cursor = "crosshair";
    });

     (metadataAreaOverlay[0]).addEventListener('mouseout', function(e) {
	document.body.style.cursor = "default";
    });




    this.updateCanvasSize();
    this.drawMetadata();
}

/**
 * Show an overlay vertical line
 * @param x the x-coordinate to show the line at
 */
metaDataHeatmapViewer.prototype.showOverlay = function(x) {
    var overlayArea = document.getElementById('metadata-area-overlay');
    var ctx = overlayArea.getContext('2d');

    var metaV = new metaDataHeatmapViewer();
    var drawConsts = metaV.getDrawConstants();

    ctx.clearRect(0,0, overlayArea.width, overlayArea.height);
    ctx.setLineDash([10,10]);
    ctx.lineWidth =1 ;

    // Within the heatmap area
    if ( x > drawConsts.left & x < drawConsts.width + drawConsts.left) {
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

    ctx.clearRect(0,0, overlayArea.width, overlayArea.height);
}

/**
 * Update the canvas size of this element with the
 * sized provided by thhe heatmapDendrogramViewer
 */
metaDataHeatmapViewer.prototype.updateCanvasSize = function() {
    console.log('metadataheatmapviewer updating canvas size');

    var metaArea =  $('#metadata-area')[0];
    var metadataAreaOverlay = $('#metadata-area-overlay')[0];

    var metadataContainer = $('#metadata-area-container');

    heatDendV = new heatmapDendrogramViewer();

    var curWidth =  heatDendV.getCurrentWidth();
    var curHeight =   heatDendV.getCurrentMetadataHeight();

    this.canvasElementWidth = curWidth;
    this.canvasElementHeight =  curHeight;

    metaArea.width = curWidth;
    metaArea.height = curHeight;

    metadataAreaOverlay.width = curWidth;
    metadataAreaOverlay.height = curHeight;

    metadataContainer.css({width: curWidth+'px', height: curHeight+'px'});
}

/**
 * Get the draw constants for the metadata heatmap
 */
metaDataHeatmapViewer.prototype.getDrawConstants = function() {
    var dendV = new dendrogramViewer();

    return {
	top: 1,
	left: heatDendView.getPlotAreaLeftPadding(),
	width: heatDendView.getPlotAreaWidth(),
	height: heatDendV.getCurrentMetadataHeight(),

    };
}

/**
 * Draw an updated metadata heatmap
 *
 * @todo This function should be broken up into a data prep and
 * data plot step
 */
metaDataHeatmapViewer.prototype.drawMetadata = function() {
    // Get cells currently displayed in the dendrogram
    var dendV = new dendrogramViewer();
    var cellSelection = dendV.getCurrentDisplayCells();

    // Some parameters to control the  drawing of the heatmap
    var plotConsts = this.getDrawConstants();

    var bottomPadding = 2;

    var top = plotConsts.top;
    var left = plotConsts.left;
    var metaWidth = plotConsts.width;
    var metaHeight = plotConsts.height - bottomPadding;


    // Request the data
    var dataCntr = new dataController();
    dataCntr.getCellMetadata(function(data) {
	// Get the canvas
	var canvas = $('#metadata-area')[0];
	var ctx =  canvas.getContext("2d");

	// Clear the canvas
	// FIXME: Clear the proper area
	ctx.clearRect(0,0,5000,5000);

	// Generate two full arrays with the things that were requested
	// One has the actual values and the other the plot colors
	// We will need the actual values for the rollovers later

	// Prepare the data for plotting
	var renderedArrayColors = Array();
	var renderedArrayValues = Array();
	var labels = Array();

	var j = 0;
	for (var key in data) {
	    // Skip if object property
	    if (! data.hasOwnProperty(key) ) continue;

	    labels[j] = key;

	    // TODO: Implement a hidden property
	    // TODO: Implement metadata row selection by the user
	    // TODO: Implement provided palette override by the user
	    // TODO: Implement default palette provision if one is not specified
	    // TODO: Add some futher checks --providing inconsistent data from server side will break this

	    curEntry =  data[key];

	    var curRow = new Array();
	    for (var i = 0; i < cellSelection.length; i++) {
		var curCell =  cellSelection[i];
		curRow[i] = curEntry.data[curCell];
	    }
	    renderedArrayValues[j] = curRow;

	    // The palette from R has an alpha channel that we
	    // have to discard
	    var fixedPalette = curEntry.palette.map(function(x) {
		return x.substring(0,7);
	    });

	    // Map to colours with provided palette
	    renderedArrayColors[j] = renderedArrayValues[j].map(function(x) {
		return (fixedPalette)[x-1];
	    });

	    j++; // Output row counter
	} // dataCntr.getCellMetadata(


	//// Plot stage
	// Computed plotting param
	var nCells = renderedArrayColors[0].length;

	// TODO: should be subtracting left and top here ( and
	//  later for the click regions as well)
	var cellWidth = metaWidth / (nCells);
	var cellHeight = metaHeight / (renderedArrayColors.length);

	var labelYpad = cellHeight / 2 + 5;
	var labelXpad = 20;

	var mdhv = new metaDataHeatmapViewer();
	mdhv.clickRegions.clearClickAreas();

	for(var i = 0; i < renderedArrayColors.length; i++) {
	    var row = renderedArrayColors[i];
	    y = i * cellHeight + top;
	    for (var j = 0; j < row.length; j++) {
		    x = (j) * cellWidth + left;
		    ctx.fillStyle =  row[j];
		    ctx.fillRect(x,y, cellWidth, cellHeight);
	    }

	    // Print names
	    var name = labels[i];

      // Cap at 16 and don't plot if smaller than 6
	    var fontSize = Math.min(cellHeight, 16);
	    if (fontSize >= 6) {
  	    ctx.font = fontSize + 'px Arial';
  	    ctx.fillStyle = 'black';
  	    var labelx = (j) * cellWidth + left + labelXpad;
  	    ctx.fillText(name, labelx , y + labelYpad);
	    }

	    // Register a click region for this metadata row
	    var y1 = i * cellHeight + top;
	    var y2 = (i + 1) * cellHeight + top;

	    var x1 = left;
	    var x2 = left + metaWidth;

	    mdhv.clickRegions.addClickArea(
		x1, y1,
		x1, y2,
		x2, y2,
		x2, y1,
		// TODO: Is there an internal name to svae here? NOPE -- but there should be
		{ metadataName: labels[i] }
	    );

	}

	// Plot a bounding box
	ctx.beginPath();
	ctx.rect(left, top, metaWidth, metaHeight);
	ctx.stroke();

    });
}
