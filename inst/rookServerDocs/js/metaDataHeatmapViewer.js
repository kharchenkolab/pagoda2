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

    metaDataHeatmapViewer.instance = this;
}

/**
 * Get the height
 */
metaDataHeatmapViewer.prototype.getHeight  = function() {
  return Ext.getCmp('metadataPanel').getHeight() - 40;
}

/**
 * Get the width
 */
metaDataHeatmapViewer.prototype.getWidth  = function() {
  return (Ext.getCmp('metadataPanel').getWidth());
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

  // Make the menu

  var toolbar = Ext.create('Ext.Toolbar');
  toolbar.add({
          text: "",
        type: "button",
        tooltip: 'Download current view',
        glyph: 0xf0ed,
        handler: function(){
            var canvas = document.getElementById('metadata-area');

                        const maxSize = 2000;
            if (canvas.width > maxSize | canvas.height >maxSize){
                Ext.Msg.show({
                  title: 'Warning',
                  msg: 'The current canvas size exceeds ' + maxSize + 'px in at least one dimention.' +
                   'This may cause problems during exporting. Do you want to continue?',
                   buttons: Ext.Msg.OKCANCEL,
                   fn: function(s) {
                     if (s == 'ok') {
                          var imageURL = canvas.toDataURL('image/png');
                          imageURL = imageURL.replace(/^data:image\/[^;]*/, 'data:application/octet-stream');
                          window.open(imageURL);
                     } //if
                   } //fn
                }) // Ext.Msg.show
            } else {
                                        var imageURL = canvas.toDataURL('image/png');
                          imageURL = imageURL.replace(/^data:image\/[^;]*/, 'data:application/octet-stream');
                          window.open(imageURL);

            }


        } // handler
  });

  toolbar.add({
  text: '',
  xtype: 'button',
  tooltip: 'Clear selection overlay',
  glyph: 0xf12d,
  handler: function() {
    var obj = new metaDataHeatmapViewer();
    obj.clearSelectionOverlay();

  }

});

  var aspectPanel = Ext.getCmp('metadataPanel').getHeader().add(toolbar);




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
 * Clears the selection overlay
 */
metaDataHeatmapViewer.prototype.clearSelectionOverlay = function() {
  var canvas = document.getElementById('metadata-area-selection');
  var ctx = canvas.getContext('2d');
  var width = canvas.width;
  var height = canvas.height;
  ctx.clearRect(0,0,width, height);
}

/**
 * Update the canvas size of this element with the
 * sized provided by thhe heatmapDendrogramViewer
 */
metaDataHeatmapViewer.prototype.updateCanvasSize = function() {
    var metaArea =  $('#metadata-area')[0];
    var metadataAreaOverlay = $('#metadata-area-overlay')[0];

    var metadataContainer = $('#metadata-area-container');

    var heatDendV = new heatmapDendrogramViewer();

    var curWidth =  this.getWidth();
    var curHeight = this.getHeight();

    this.canvasElementWidth = curWidth;
    this.canvasElementHeight =  curHeight;

    metaArea.width = curWidth;
    metaArea.height = curHeight;

    metadataAreaOverlay.width = curWidth;
    metadataAreaOverlay.height = curHeight;


    var metadataAreaSelection = $('#metadata-area-selection')[0];
    metadataAreaSelection.width = curWidth;
    metadataAreaSelection.height = curHeight;


    metadataContainer.css({width: curWidth+'px', height: curHeight+'px'});
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
metaDataHeatmapViewer.prototype.drawMetadata = function() {
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
    	var ctx =  canvas.getContext("2d");

    	// Clear the canvas
    	// FIXME: Clear the proper area
    	ctx.clearRect(0,0,5000,5000);

      // Get number of cells and calculate cell size
    	var nCells = cellSelection.length;
      var nrows = Object.keys(data).length;

      // Calculate cell size
    	var cellWidth = metaWidth / (nCells);
    	var cellHeight = metaHeight / nrows;

      // Calculate label position
    	var labelYpad = cellHeight / 2 + 5;
    	var labelXpad = 20;

      // Get  and clear the click areas
    	var mdhv = new metaDataHeatmapViewer();
    	mdhv.clickRegions.clearClickAreas();

      // For each metadata row
      var j =0; // row counter
    	for (var key in data) {
    	    // Skip if object property
    	    if (! data.hasOwnProperty(key) ) continue;

          // Current plotting information
          var curLabel = data[key].displayname;
          if (typeof curLabel === 'undefined') {curLabel = ''}; // default to empty

    	    var curData =  data[key].data;
          var curPal = data[key].palette;

          // Get the y coordinate
          var y = j * cellHeight + top;


    	    // Plot the row in the order provided by the dendrogram
    	    for (var i = 0; i < cellSelection.length; i++) {
        		var curCell =  cellSelection[i];

        		// value to plot
        		var val = curData[curCell]; // value
            var col = curPal[val];

            if (typeof col == 'undefined') {
                console.warn('Level without a corresponding color found');
                col = '#FFFFFFFF'; // Leave a gap
            }


            var x = i * cellWidth + left;
		        ctx.fillStyle = col.substr(0,7); // color w/o alpha
		        ctx.fillRect(x,y, cellWidth, cellHeight);
    	    }

    	    // Plot the label
    	    // Cap at 16 and don't plot if smaller than 6
    	    var fontSize = Math.min(cellHeight, 16);
    	    if (fontSize >= 6) {
      	    ctx.font = fontSize + 'px Arial';
      	    ctx.fillStyle = 'black';
      	    var labelx = cellSelection.length * cellWidth + left + labelXpad;
      	    ctx.fillText(curLabel, labelx , y + labelYpad);
    	    }

    	    // Register a click region for this metadata row
    	    var y1 = j * cellHeight + top;
    	    var y2 = (j + 1) * cellHeight + top;

    	    var x1 = left;
    	    var x2 = left + metaWidth;

    	    mdhv.clickRegions.addClickArea(
        		x1, y1,
        		x1, y2,
        		x2, y2,
        		x2, y1,
        		{ metadataName: key }
    	    );

          // Increment row counter
    	    j++
    	} // dataCntr.getCellMetadata

    	// Plot a bounding box
    	ctx.beginPath();
    	ctx.rect(left, top, metaWidth, metaHeight);
    	ctx.stroke();

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
  var dendV = new dendrogramViewer();

    // Get the cells in the cell selection to highlight
  var cellSelCntr = new cellSelectionController();
  cellSelection = cellSelCntr.getSelection(selectionName);


  // Get the cell order
  var dataCntr = new dataController();
  dataCntr.getCellOrder(function(cellorder) {
    // Currently displayed cells
    var cellRange = dendV.getCurrentDisplayCellsIndexes();
    var ncells = cellRange[1] - cellRange[0];

    var ctx = metadataHeatV.getSelectionDrawingContext();
    ctx.clearRect(0,0,3000,3000);

var heatDendView = new heatmapDendrogramViewer();
    // Get and calculate plotting values
    var drawConsts = metadataHeatV.getDrawConstants();
    var heatmapWidth = drawConsts.width - heatDendView.getPlotAreaRightPadding();
    var cellWidth = heatmapWidth / ncells;
    var left = drawConsts.left;
    var n = cellSelection.length;

     var actualPlotHeight = drawConsts.height;

    ctx.save();
    ctx.strokeStyle = 'rgba(255,0,0,0.3)';


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

  }); // get the cell order
}


