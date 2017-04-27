/*
 * Filename: aspectHeatmapViewer.js
 * Author: Nikolas Barkas
 * Description: implements the aspect heatmap viewer for pagoda2
 */

/**
 * Object that manages the aspect heatmap viewer
 */
function aspectHeatmapViewer() {
  if (typeof aspectHeatmapViewer.instance === 'object') {
    return aspectHeatmapViewer.instance;
  }
  console.log('Initializing aspect heatmap viewer...');

  aspectHeatmapViewer.instance = this;
}


/**
 * Perform initialization of the aspect heatmap viewer
 * @description This is called by the parent heatmapDendrogram object
 * when initialization has finished.
 */
aspectHeatmapViewer.prototype.initialize = function() {

  // Clickable regions for cross hairs and navigating
  this.aspectRegions = new clickableRegions();

  var aspectHeatmapContainer = $('#aspect-heatmap-container');
  aspectHeatmapContainer.css({position: 'relative'});

  aspectHeatmapContainer.append(
    '<canvas id="aspect-heatmap-area"></canvas>' +
    '<canvas id="aspect-heatmap-area-overlay"></canvas>'
  );

  var aspectHeatmapArea = $('#aspect-heatmap-area');
  aspectHeatmapArea.css({
    position: 'absolute',
    top: 0,
    left: 0
  });

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
      this.palManager = new paletteManager();
    this.palManager.setPalette(p2globalParams.heatmapViewer.defaultPaletteName);
    this.palManager.setNumberOfColors(p2globalParams.heatmapViewer.defaultPaletteLevels);

  // Draw the heatmap
  this.drawHeatmap();

};

/**
 * Setup the event listeners for the overlay effects
 */
aspectHeatmapViewer.prototype.setupOverlays = function() {
  var heatmapOverlayArea = $('#aspect-heatmap-area-overlay')[0];
  var aspHeatView = this;

  heatmapOverlayArea.addEventListener('click', function(e) {
    var x = e.layerX;
    var y = e.layerY;

    var regionData = aspHeatView.aspectRegions.resolveClick(x,y);
    if (typeof regionData !== 'undefined') {
      console.log(regionData);

      // TODO: Uncomment when embedding viewer supports this
      /*
      var embV = new embeddingViewer();
	    embV.setColorConfiguration('aspect');
	    embV.setAspectColorInfo({aspectid: regionData.aspectId});
	    embV.updateColors();
	    */
    };
  }); // click listener

  heatmapOverlayArea.addEventListener('mousemove', function(e) {

    var aspHeatView =  new aspectHeatmapViewer();

    var x = e.layerX;
    var y = e.layerY;

  	var heatV = new heatmapViewer();
  	var metaV = new metaDataHeatmapViewer();

  	heatV.showOverlay(x);
  	metaV.showOverlay(x);

  	aspHeatView.showOverlay(x, y);

  });

   heatmapOverlayArea.addEventListener('mouseenter', function(e) {
     document.body.style.cursor = "crosshair";
   });

   heatmapOverlayArea.addEventListener('mouseout', function(e) {
     var aspHeatView =  new aspectHeatmapViewer();
     aspHeatView.clearOverlay();
     document.body.style.cursor = "default";
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
  ctx.clearRect(0,0,width,height);
}

/**
 * Show overlay for specific coordinates
 */
aspectHeatmapViewer.prototype.showOverlay = function(x,y) {
  var aspHeatView = new aspectHeatmapViewer()

  var overlayArea = document.getElementById('aspect-heatmap-area-overlay');
  var ctx = overlayArea.getContext('2d');

  var drawConsts = aspHeatView.getDrawConstants();

  var areaWidth = overlayArea.width;
  var areaHeight = overlayArea.height;

  ctx.setLineDash([10,10])
  ctx.lineWidth = 1;
  ctx.clearRect(0,0,areaWidth,areaHeight);

  var actualPlotHeight = this.getActualPlotHeight();

  if (typeof y !== 'undefined' & y < actualPlotHeight & y > drawConsts.top){
    ctx.beginPath();
    ctx.moveTo(drawConsts.left, y);
    ctx.lineTo(drawConsts.width + drawConsts.left, y);
    ctx.stroke();
  }

  if (typeof x !== 'undefined' & x > drawConsts.left & x < drawConsts.width + drawConsts.left  &
	    (y < actualPlotHeight  | typeof y === 'undefined') // if y is provided it is in the plot
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
  heatDendV = new heatmapDendrogramViewer();
  var curWidth = heatDendV.getCurrentWidth();
  var curHeight = heatDendV.getCurrentAspectHeatmapHeight();

  var containerDiv = $('#aspect-heatmap-container');
  containerDiv.css({
    'width': curWidth,
    'height': curHeight
  });

  this.canvasElementWidth = curWidth;
  this.canvasElementHeight= curHeight;

  // Update the size of teh main area
  aspectHeatmapArea.width = curWidth;
  aspectHeatmapArea.height = curHeight;

  // Update the size of the overaly
  aspectHeatmapAreaOverlay.width = curWidth;
  aspectHeatmapAreaOverlay.height = curHeight;
}

/**
 * Clear the heatmap
 */
aspectHeatmapViewer.prototype.clearHeatmap = function(ctx) {
  ctx.clearRect(0,0,this.width,this.height);
}

/**
 * Draw the heatmap. Will clear heatmap if required
 */
aspectHeatmapViewer.prototype.drawHeatmap = function() {
	var aspHeatView = this;
  var heatDendView = new heatmapDendrogramViewer();
	var dendV = new dendrogramViewer();
	ctx = this.getDrawingContext();

	drawConsts = this.getDrawConstants();
	var top = drawConsts.top;
	var left = drawConsts.left;
	var heatmapWidth  = drawConsts.width;
	var heatmapHeight = drawConsts.height - drawConsts.paddingBottom;

	// Get the cells to plot
	var cellRange = dendV.getCurrentDisplayCellsIndexes();
	var cellIndexStart = cellRange[0];
	var cellIndexEnd = cellRange[1];

	this.clearHeatmap(ctx);

	// Show centered waiting icon
	$('#aspect-heatmap-container').append("<img class='loadingIcon' src='img/loading.gif'/>");
	var loadingDomItem =  $('#aspect-heatmap-container > .loadingIcon');
  var lpad = heatDendView.getCurrentWidth()  / 2;
  var tpad = heatDendView.getCurrentHeatmapHeight() /2;
  loadingDomItem.css({'padding-left': lpad + 'px', 'padding-top': tpad + 'px'});

	var dataCntr = new dataController();
	dataCntr.getAspectMatrix(cellIndexStart, cellIndexEnd, false, function(data){

	  loadingDomItem.remove();

    var naspects = data.Dim[1];
    var ncells = data.Dim[0];

    // Computed plotting params
    var cellWidth = heatmapWidth / ncells;
    var cellHeight = heatmapHeight / naspects;

    var actualPlotHeight = heatmapHeight; // for convinience
    aspHeatView.setActualPlotHeight( actualPlotHeight );

    var palSize = aspHeatView.palManager.getNumberOfColors();
    var pal = aspHeatView.palManager.getPaletteColors();


	  ctx.fillStyle = pal[Math.floor(palSize/2)]
	  ctx.fillRect(left, top, heatmapWidth, actualPlotHeight);

    for (var j = 0; j < data.p.length -1; j++) {
      var rsi = data.p[j];
      var rei = data.p[j+1] - 1;

      if (rsi === rei) {continue;};

      // Calculate row normalisation
      var rowMin = data.x.slice(rsi, rei).reduce( function(a,b){ return Math.min(a,b)} );
	    var rowMax = data.x.slice(rsi, rei).reduce(function(a,b){ return Math.max(a,b) } );
	    var rowSum = data.x.slice(rsi, rei).reduce(function(a,b){ return a+b });
      var rowMean = rowSum / (rei - rsi + 1);
	    var maxAbsValue = Math.max(Math.abs(rowMin - rowMean), Math.abs(rowMax - rowMean));

      // Color mapper for this row
      var colorMapper = aspHeatView.palManager.getMeanClampedColorMapper(rowMean, maxAbsValue, palSize);

      for (var k = rsi; k < rei; k++){
        var palIndex = colorMapper(data.x[k]);
        ctx.fillStyle = pal[palIndex];

        var x = data.i[k] * cellWidth + left;
		    var y = j * cellHeight + top;

		    ctx.fillRect(x,y, cellWidth, cellHeight);
	    } // for k


    } // for j

    // Plot the bounding box
    ctx.strokeRect(left, top, heatmapWidth, actualPlotHeight);


    // TODO: setup click areas here
    aspHeatView.aspectRegions.clearClickAreas();
    for (var i = 0; i < data.DimNames2.length; i++){
      var x1 = left;
	    var y1 = cellHeight * i + top;
	    var x2 = heatmapWidth;
	    var y2 = cellHeight * ( i + 1 ) + top;

	    aspHeatView.aspectRegions.addClickArea(
    		x1, y1,
    		x1, y2,
    		x2, y2,
    		x2, y1,
    		{aspectId: data.DimNames2[i] }
	    );
    }


	}); // dataController callback

};


/**
 * Get 2d drawing context for the canvas
 */
aspectHeatmapViewer.prototype.getDrawingContext = function() {
  return document.getElementById('aspect-heatmap-area').getContext('2d');
}

/**
 * Get drawing constants for drawing
 */
aspectHeatmapViewer.prototype.getDrawConstants = function() {
    var heatDendView = new heatmapDendrogramViewer();

    // TODO: values here should be in global params
    return {
    	top: 5,
    	left:  heatDendView.getPlotAreaLeftPadding(),
    	width: heatDendView.getPlotAreaWidth(),
    	height: heatDendView.getCurrentAspectHeatmapHeight(),
    	paddingBottom: 10,
    	maxCellHeight: 30,
    }
}

