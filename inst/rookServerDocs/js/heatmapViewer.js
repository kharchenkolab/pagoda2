/*
 * Filename: heatmapViewer.js
 * Author: Nikolas Barkas
 * Description: implements the heatmap viewer for pagoda2
 */

/**
 * Manages the heatmap viewer
 * @constructor
 */
function heatmapViewer() {
    if (typeof heatmapViewer.instance ===  'object') {
	return heatmapViewer.instance;
    }
    console.log('Initializing heatmap viewer...');

    // NOTE: Actual init is done by parent object

    var extJsContainer = Ext.getCmp('heatmapPanel');
    extJsContainer.onResize = function() {
    	var heatView = new heatmapViewer();
    	heatView.updateCanvasSize();
    	heatView.drawHeatmap();
    };

    heatmapViewer.instance =  this;
};


/**
 * Perform initialization of the heatmap viewer.
 * @description This is called by the
 * parent heatmapDendrogram viewer object when the dendrogram has finished
 * loading. This is dore because it is the dendrogram object that will
 * provide order for the columns here -- we can't rely on the backend
 * to do this because we will eventually need to do this clientside
 */
heatmapViewer.prototype.initialize = function() {
    // TODO: get a variable to check that this is not run twice

    // Make a clickable regions object that we will uses for the crosshair gene name
    this.geneRegions = new clickableRegions();


    // Generate our canvases

    // NOTE: We are putting two canvases in #heatmap-area-container
    // and we are positioning them directly on top of each other
    // So we can have our static heatmap on the bottom and dynamic
    // annotation (like crosshairs) on top without having
    // to redraw the bottom repeatedly

    var heatmapContainer = $('#heatmap-area-container');
    heatmapContainer.css({position: 'relative'});

    heatmapContainer.append(
    	'<canvas id="heatmap-area" ></canvas>' +
    	'<canvas id="heatmap-area-overlay"></canvas>'
    );

    var heatmapArea = $('#heatmap-area');

    heatmapArea.css({
    	position: 'absolute',
    	top: 0,
    	left: 0
    });

    var heatmapAreaOverlay = $('#heatmap-area-overlay');
    heatmapAreaOverlay.css({
      	position: 'absolute',
      	top: 0,
      	left: 0,
    });

    // Setup the events handling the overlay effect of the canvas
    this.setupOverlays();

    // Update the size of both canvases
    this.updateCanvasSize();

    // initialise the display genes array
    this.displayGenes = [];

    // Palette Manager init
    this.palManager = new paletteManager();
    this.palManager.setPalette(p2globalParams.heatmapViewer.defaultPaletteName);
    this.palManager.setNumberOfColors(p2globalParams.heatmapViewer.defaultPaletteLevels);

    this.displayGenes = new Array();
    this.drawHeatmap();

    this.setRowReordering(p2globalParams.heatmapViewer.defaultRowReordering);

    this.generateMenu();
};



/**
 * Generate the palettes menu
 * @private
 * @returns palette menu extjs object
 */
heatmapViewer.prototype.generatePalettesMenu = function() {
    var paletteChangeHandler = function(item) {
        var heatView = new heatmapViewer();

        heatView.palManager.setPalette(item.value);

    		// NOTE: We are getting  the number of colors because the
    		// Manger will have sorted out any issues with exceeeding the
    		// new palette limits
    		var curNoColours = heatView.palManager.getNumberOfColors();

        // Set the actual value to the menu
        Ext.getCmp('paletteLevelsField').setValue(curNoColours);

        // Impose the new limits of this palette
        Ext.getCmp('paletteLevelsField').setMinValue(heatView.palManager.getMinNumberOfColors());
        Ext.getCmp('paletteLevelsField').setMaxValue(heatView.palManager.getMaxNumberOfColors());

    		heatView.drawHeatmap();
    };


    var palettes = p2globalParams.heatmapViewer.availablePalettes;
    var paletteMenu = Ext.create('Ext.menu.Menu');
    for (i in palettes)    {
        paletteMenu.add({
            text: palettes[i].displayName,
	          value: palettes[i].name,
            handler: paletteChangeHandler
	    }); // paletteMenu.add
    } // for
    return paletteMenu;
}



/**
 * Generates the heatmap configuration menu
 */
heatmapViewer.prototype.generateMenu = function() {
  var toolbar = Ext.create('Ext.Toolbar');
var heatView = this;

var paletteMenu = this.generatePalettesMenu();

 var heatmapSettingsMenu = Ext.create('Ext.menu.Menu', {
	id: 'heatmapSettingsMenu',
	items: [
	    {
		text: 'Palette Name',
		menu: paletteMenu
	    },
	    {
		fieldLabel: 'Palette Levels',
		id: 'paletteLevelsField',
		xtype: 'numberfield',
		tooltip: 'Number of colors for the palette',
		value: p2globalParams.heatmapViewer.defaultPaletteLevels, // FIXME
		disabled: false,
		maxValue: heatView.palManager.getMaxNumberOfColors(),
		minValue: heatView.palManager.getMinNumberOfColors(),
		listeners: {
		    change: {buffer: 800, fn: function(f,v) {
      			var heatView = new heatmapViewer();
      			heatView.palManager.setNumberOfColors(v);
      			heatView.drawHeatmap();

		    }} // buffer of change listener
		}
	    },
	    {
		text: 'Reorder rows',
		checked: p2globalParams.heatmapViewer.defaultRowReordering,
		checkHandler: function(e, checked, eOpts) {
		    var heatV =  new heatmapViewer();
		    heatV.setRowReordering(checked);
		    heatV.drawHeatmap();
		}
	    }

	] // items
    });

toolbar.add({
          text: "",
        type: "button",
        tooltip: 'Download current view',
        glyph: 0xf0ed,
        handler: function(){
            var canvas = document.getElementById('heatmap-area');
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

            }// if
        } // handler
});

      // Add plot configuration menu button
    toolbar.add({
    	text: '',
    	xtype: 'button',
    	tooltip: 'Configure heatmap plot settings',
    	glyph: 0xf013,
    	menu: heatmapSettingsMenu
    });


    var heatmapPanel = Ext.getCmp('heatmapPanel');
    heatmapPanel.getHeader().add(toolbar);
}

/**
 * Setup all the overlay canvas events
 * @description sets up all the events on the overlay canvas
 * that used for things  like crosshairs, tooltip and gene row
 * clicking
 */
heatmapViewer.prototype.setupOverlays = function() {

    var heatmapAreaOverlay = $('#heatmap-area-overlay')[0];

    // Click listener for setting gene color to embedding
    heatmapAreaOverlay.addEventListener('click', function(e) {
	var x = e.layerX;
	var y = e.layerY;

	var heatView = new heatmapViewer();
	var regionData = heatView.geneRegions.resolveClick(x, y);

	// Draw tooltip
	if (typeof regionData !== 'undefined') {
	    // Tell the embedding to update
	    var embV = new embeddingViewer();
	    embV.setColorConfiguration('geneexpression');
	    embV.setGeneExpressionColorInfo({geneid: regionData.geneId});
	    embV.updateColors();
	}
    });

    // Mouse  move listener for the cross hairs and tooltip
    heatmapAreaOverlay.addEventListener('mousemove', function(e) {
	var x = e.layerX;
	var y = e.layerY;

	var heatV = new heatmapViewer();
	var metaV = new metaDataHeatmapViewer();
	var aspeV = new aspectHeatmapViewer();

	var label;

	var regionData = heatV.geneRegions.resolveClick(x, y);
	if  (typeof regionData !== 'undefined') {
	    label = 'Gene: ' + regionData.geneId;
	}

	heatV.showOverlay(e.layerX, e.layerY, label);
	metaV.showOverlay(e.layerX);
	aspeV.showOverlay(e.layerX);


    });

    // Remove the cursor when done
    heatmapAreaOverlay.addEventListener('mouseout', function(e) {
	var metaV = new metaDataHeatmapViewer();
	var heatV = new heatmapViewer();

	heatV.clearOverlay();
	metaV.clearOverlay();
    });

    // Pointer change to cross hairs when over the heatmap
    heatmapAreaOverlay.addEventListener('mouseenter', function(e) {
	document.body.style.cursor = "crosshair";
    });

    heatmapAreaOverlay.addEventListener('mouseout', function(e) {
	document.body.style.cursor = "default";
    });
}

/**
 * Get the order in which the rows of the provided data
 * are best ordered in for visual purposes
 * @param data a dgCMatrix reader with the sparse array data
 */
heatmapViewer.prototype.getRowVisualOrder = function(data) {
//    the bin size


    var ncols = data.Dim[0];
    var nrows = data.Dim[1];


    var binsize = Math.max(ncols / 30);
    var ncolsbin = Math.ceil(ncols / binsize);

    // Make empty bins array
    var binsSumArray = new Array(nrows);
    for (var k = 0; k < nrows; k++) {
    	var row = new Array(ncolsbin);
    	for (var j = 0; j < ncolsbin; j++) {
    	    row[j] = 0;
    	}
    	binsSumArray[k] = row;
    }

    // Sum values for every bin
    for (var j = 0; j < data.p.length - 1; j++) {
    	// Row start and end index
    	var rsi = data.p[j];
    	var rei = data.p[j+1] - 1;

    	for (var k = rsi; k < rei; k++) {
    	    var cn = data.i[k];

    	    // Find the bin index for this element
    	    var binindex = Math.floor(cn / binsize);
    	    binsSumArray[j][binindex] = binsSumArray[j][binindex] + data.x[k];
    	}
    }

    function meanClampNorm(v, rowMean, maxAbsValue) {
    	const trim = 0.3;
    	maxAbsValue *= trim;

    	const range = 1;

    	var val =  (v - rowMean) / (maxAbsValue * 2) + 0.5;
    	val = val * range;
    	val = val < 0 ? 0 : val;
    	val = val > range ? range : val;
    	return val;
    }

    // Normalise
    for (var j = 0; j < binsSumArray.length; j++) {

    	var maxAbsValue = 0;

    	var rowMean = 0;
    	var row = binsSumArray[j];
    	for (var i = 0; i < row.length; i++) {
    	    rowMean += row[i];
    	    maxAbsValue =  Math.abs(row[i]) > maxAbsValue ? Math.abs(row[i]) : maxAbsValue;
    	}
    	rowMean /= row.length;

    	for (var i =0; i< row.length; i++) {
    	    row[i] = meanClampNorm(row[i], rowMean, maxAbsValue);
    	}

    	// This is probably not requried
    }

    d = binsSumArray;

    for (var j = 0; j < d.length; j++) {
	var maxAbsValue = 0;
	var rowMean = 0;
	for (var i = 0; i < d[j].length; i++) {
	    rowMean += d[j][i];
	    maxAbsValue = Math.abs(d[j][i]) > maxAbsValue ? Math.abs(d[j][i]) : maxAbsValue;
	}
	rowMean /= d[j].length;
	for (var  i =0; i < d[j].length; i++) {
	    d[j][i] = meanClampNorm(d[j][i], rowMean, maxAbsValue);
	}
    }

    // Do hierarchical clustering
    hc = hcluster(d, pagHelpers.seq(0, d.length - 1), 'corrdist', 'average')
    // Do a depth first search on the tree
    function getOrder(hc) {
	var order = [];
	if (hc.hasOwnProperty('left')) {
	    order = order.concat(getOrder(hc.left));
	    order = order.concat(getOrder(hc.right));
	} else {
	    order.push(hc.label);
	}
	return order;
    }
    order = getOrder(hc);

    // Change into the required format
    var order2 = [];
    for (var i = 0; i < order.length; i++) {
	order2[order[i]] = i;
    }


    // Return the order from the depth first
    return order2;

}

/**
 * Clear the overlay
 */
heatmapViewer.prototype.clearOverlay = function() {
  	var overlayArea = document.getElementById('heatmap-area-overlay');
	var ctx = overlayArea.getContext('2d');

	var width = overlayArea.width;
	var height = overlayArea.height;

	ctx.clearRect(0,0,width, height);
}

/**
 * Show the overlay crosshairs and tooltip
 */
heatmapViewer.prototype.showOverlay = function (x,y, label) {

    // Avoid jquery lookup for performance here
    var overlayArea = document.getElementById('heatmap-area-overlay');
    var ctx = overlayArea.getContext('2d');

    var heatView = new heatmapViewer();

    var drawConsts = heatView.getDrawConstants();

    var areaWidth = overlayArea.width;
    var areaHeight = overlayArea.height;

    ctx.setLineDash([10,10])
    ctx.lineWidth = 1;
    ctx.clearRect(0,0,areaWidth,areaHeight);

    var actualPlotHeight = heatView.getActualPlotHeight();

    if (typeof y !== 'undefined' & y < actualPlotHeight
       ){
	ctx.beginPath();
	ctx.moveTo(drawConsts.left, y);
	ctx.lineTo(drawConsts.width + drawConsts.left, y);
	ctx.stroke();
    }

    if (typeof x !== 'undefined' &
	x > drawConsts.left &
	x < drawConsts.width + drawConsts.left  &
	(y < actualPlotHeight  | typeof y === 'undefined') // if y is provided it is in the plot
       ) {

	ctx.beginPath();
	ctx.moveTo(x, drawConsts.top);
	ctx.lineTo(x, actualPlotHeight + drawConsts.top);
	ctx.stroke();
    }

    // The tooltip
    if (typeof y !== 'undefined' & typeof x !== 'undefined' & typeof label !== 'undefined') {

	ctx.save();
	const tooltipTop = 30;
	const tooltipLeft = 30;

	const tooltipWidthPadding = 20;
	const tooltipHeight = 30;

	const tooltipShadowOffset = 3;

	ctx.font = '18px Arial';
	var textSize = ctx.measureText(label);

	// Tooltip text
	ctx.fillStyle = 'black';
	ctx.fillText(label, x + tooltipLeft , y + tooltipTop);
	ctx.restore();
    }
}

heatmapViewer.prototype.getHeight = function() {
  return Ext.getCmp('heatmapPanel').getHeight() - 60;
}

heatmapViewer.prototype.getWidth = function() {
  return (Ext.getCmp('heatmapPanel').getWidth() );
}

/**
 * Update the canvas size given the size from heatmapDendrogram
 */
heatmapViewer.prototype.updateCanvasSize = function() {
    var heatmapArea = $('#heatmap-area')[0];
    var heatmapAreaOverlay = $('#heatmap-area-overlay')[0];

    var curWidth =  this.getWidth();
    var curHeight =  this.getHeight();

    this.canvasElementWidth = curWidth;
    this.canvasElementHeight =  curHeight;

    // Update the size of  the  main area
    heatmapArea.width = curWidth;
    heatmapArea.height = curHeight;

    // Update the  size of the overlay
    heatmapAreaOverlay.width = curWidth;
    heatmapAreaOverlay.height = curHeight;
}

/**
 * Sets a gene selection from the gene selection controller as the
 * display genelist
 * @param selectionName Name of the selection from the gene selection controller
 */
heatmapViewer.prototype.setNamedSelectionToDisplayGenes = function(selectionName) {
    var geneSelCntr =  new geneSelectionController();
    this.displayGenes = geneSelCntr.getSelection(selectionName).genes;
}


/**
 * Return the font size to use to print the heatmap row names
 */
heatmapViewer.prototype.getRowFontSize = function (cellHeight) {
    var a = Math.min(cellHeight, 20);
    a = a * 3/4;
    if (a < 8) { a = 0;}

    return a;
}

/**
 * set the row reordering
 */
heatmapViewer.prototype.setRowReordering = function(v) {
    this.rowReordering = v;
}


/**
 * get the row reordeing
 */
heatmapViewer.prototype.getRowReordering = function(v) {
    return this.rowReordering;
}


/**
 * Clears the heatmap drawing area
 */
heatmapViewer.prototype.clearHeatmap = function(ctx) {
	// Clear the canvas
	// FIXME: Clear the correct area
	ctx.clearRect(0,0,3000,3000);
}

/**
 * Draws the heatmap
 */
heatmapViewer.prototype.drawHeatmap = function() {
    if (typeof this.displayGenes === 'undefined' || this.displayGenes.length === 0) {
	// No gene selected

	ctx = this.getDrawingContext();
	this.clearHeatmap(ctx);

	var heatDendView = new heatmapDendrogramViewer();

	var left = heatDendView.getPlotAreaLeftPadding();
	var heatmapWidth = this.getWidth() - heatDendView.getPlotAreaRightPadding();
	var heatmapHeight = this.getHeight();

	const text = 'No Genes selected';

	ctx.font = '20px Arial';

	// Center justify the lest
	var measure = ctx.measureText(text);
	left = left - measure.width /2;
	ctx.fillText(text, heatmapWidth /2 + left, heatmapHeight/2);


	// We also want to maintain a gene selection showing the
	// current plotted genes
	var geneSelCntr = new geneSelectionController();
	geneSelCntr.setSelection('heatmapDisplayGenes', [], 'Current Heatmap Genes');
    } else {

	// Make a gene selection with the currently shown genes
	var geneSelCntr = new geneSelectionController();
	geneSelCntr.setSelection('heatmapDisplayGenes', this.displayGenes, 'Current Heatmap Genes');
	this.doDrawHeatmap();
    }
}

/**
 * Get 2d drawing context for the main heatmap canvas
 * @private
 */
heatmapViewer.prototype.getDrawingContext = function() {
    // Get the plotting context
    var canvas = $('#heatmap-area')[0];
    var ctx = canvas.getContext("2d");

    return ctx;
}

heatmapViewer.prototype.getDrawConstants = function() {
    var heatDendView = new heatmapDendrogramViewer();

    // TODO: values here should be in global params
    return {
    	top: 5,
    	left:  heatDendView.getPlotAreaLeftPadding(),
    	width: this.getWidth() - heatDendView.getPlotAreaRightPadding(),
    	height: this.getHeight() ,
    	paddingBottom: 10,
    	maxCellHeight: 30,
    }
}

/**
 * Set the plot height that was actually used
 */
heatmapViewer.prototype.setActualPlotHeight = function(val) {
    this.actualPlotHeight = val;
}

/**
 * Get the plot height that was actually used
 */
heatmapViewer.prototype.getActualPlotHeight = function() {
    return this.actualPlotHeight;
}


/**
 * Plot the heatmap internally using the appropriate function
 * @private
 */
heatmapViewer.prototype.doDrawHeatmap = function() {
    // TODO: Left over, remove
    this.doDrawHeatmapSparseMatrix();
}


/**
 * Internal function for drawing the heatmap using the sparse matrix directly
 * @description assumes that the displayGenes in is not empty
 * @private
 */
heatmapViewer.prototype.doDrawHeatmapSparseMatrix = function() {

    var dendV = new dendrogramViewer();
    var heatDendView = new heatmapDendrogramViewer();


// heatDendView.getCurrentHeatmapHeight()

    // Get the drawing params
    drawConsts = this.getDrawConstants();
    var top = drawConsts.top;
    var left = drawConsts.left;
    var heatmapWidth = drawConsts.width;
    var heatmapHeight = drawConsts.height - drawConsts.paddingBottom;

    // Get the genes to plot
    var geneSelection = this.displayGenes;

    // Get the cells to plot
    var cellRange = dendV.getCurrentDisplayCellsIndexes();
    var cellIndexStart = cellRange[0];
    var cellIndexEnd = cellRange[1];

    var heatView = this;

    // Clear heatmap
    var ctx = heatView.getDrawingContext();
    heatView.clearHeatmap(ctx);

    // Show centered waiting icon
    $('#heatmap-area-container').append("<img class='loadingIcon' src='img/loading.gif'/>");
    var loadingDomItem =  $('#heatmap-area-container > .loadingIcon')
    var lpad = this.getWidth()  / 2;
    var tpad = this.getHeight() /2;
    loadingDomItem.css({'padding-left': lpad + 'px', 'padding-top': tpad + 'px'});

    var dataCntr = new dataController();
    dataCntr.getExpressionValuesSparseByCellIndexUnpacked(geneIds = geneSelection, cellIndexStart = cellIndexStart, cellIndexEnd = cellIndexEnd, getCellNames = false, callback =  function(data) {
        loadingDomItem.remove();

	// Get the dimensions
	var ngenes = data.Dim[1];
	var ncells = data.Dim[0];

	// Computed plotting params
	var cellWidth = heatmapWidth / ncells;
	var cellHeight = heatmapHeight / ngenes;

	// Limit max cell height
	drawConsts = heatView.getDrawConstants(); // Do we need this again
	cellHeight = Math.min(cellHeight, drawConsts.maxCellHeight);

	// Calculate Height after taking max cell size to account
	var actualPlotHeight = ngenes * cellHeight;

	// Have to set it on the obj for the crosshairs
	heatView.setActualPlotHeight( actualPlotHeight );

	// Get a new row order
	var rowOrder;
	if (heatView.getRowReordering() === true) {
            rowOrder = heatView.getRowVisualOrder(data);
	} else {
	    rowOrder = pagHelpers.seq(0,data.Dim[1] -1);
	}

	// Get palette
	var palSize = heatView.palManager.getNumberOfColors();
	var pal = heatView.palManager.getPaletteColors();

	// Plot background
	ctx.fillStyle = pal[Math.floor(palSize/2)];
	ctx.fillRect(left,top,heatmapWidth,actualPlotHeight);

	for ( var j = 0; j < data.p.length - 1; j++) {
	    // row start index, row end index (in x and i)
	    var rsi = data.p[j];
	    var rei = data.p[j+1] -1;

	    // Calculate row normalisation values
	    var rowMin = data.x.slice(rsi, rei).reduce(function(a,b){ return Math.min(a,b) } );
	    var rowMax = data.x.slice(rsi, rei).reduce(function(a,b){ return Math.max(a,b) } );
	    var rowSum = data.x.slice(rsi, rei).reduce(function(a,b){ return a+b });
	    var rowMean = rowSum / (rei - rsi + 1);
	    var maxAbsValue = Math.max(Math.abs(rowMin - rowMean), Math.abs(rowMax - rowMean));

	    // color mapper is a function
	    // use a color mapper to ensure consistency of coloring with
	    // other views (eg embedding)
	    var colorMapper = heatView.palManager.getMeanClampedColorMapper(rowMean, maxAbsValue, palSize);

	    // Plot row
	    for (var k = rsi; k < rei; k++) {
//		var plotValue = (data.x[k] - rowMean) / (maxAbsValue * 2) + 0.5;
//		var palIndex = Math.floor(plotValue * (palSize)) - 1;

		var palIndex = colorMapper(data.x[k]);
		ctx.fillStyle = pal[palIndex];

		var x = data.i[k] * cellWidth + left;
		var y = rowOrder[j] * cellHeight + top; // reorder on the fly

		ctx.fillRect(x,y,
		    cellWidth, cellHeight);
	    } // for k

	} // for j

	// Draw bounding box
	ctx.strokeRect(left, top, heatmapWidth, actualPlotHeight);

	// Plot the labels
	for (var i = 0; i < data.DimNames2.length; i++) {
	    // Print the names
	    var name = data.DimNames2[i];

	    // Get the appropriate font size for this number of cells
	    var fontSize =  heatView.getRowFontSize(cellHeight)

	    // Calculate position
	    x = ncells * cellWidth + left + 20;
	    y = rowOrder[i] * cellHeight + top + cellHeight / 2 + fontSize / 3;

	    // Plot
	    ctx.font =  fontSize + 'px Arial';
	    ctx.fillStyle = 'black';
	    ctx.fillText(name, x, y);
	} // Label plot loop


	// Setup the resolvable regions (to gene names)
	// these are used for tooltips and coloring by gene
	heatView.geneRegions.clearClickAreas();
	for (var i = 0; i < data.DimNames2.length; i++) {
	    j = rowOrder[i];

	    var x1 = left;
	    var y1 = cellHeight * j + top;
	    var x2 = heatmapWidth;
	    var y2 = cellHeight * ( j +1 ) + top;

	    heatView.geneRegions.addClickArea(
		x1, y1,
		x1, y2,
		x2, y2,
		x2, y1,
		{geneId: data.DimNames2[i] }
	    );
	} // for


    }); // dataCntr.getExpressionValuesSparseTransposedByCellIndexUnpacked callback
} // doDrawHeatmapSparseMatrix


