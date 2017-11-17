"use strict";
/*
 * Filename: heatmapDendrogramViewer.js
 * Author:  Nikolas Barkas
 * Date:  March 2017
 * Description: heatmap and dendrogram viewer for pagoda 2
 */

/**
 * Heatmap and dendrogram viewer
 * @constructor
 *
 * @description This object is responsible for managing the heatmap
 * and the  dendrogram viewer. The actual plots are managed
 * by independent objects, but this class coordinates them
 * to allow for things like zooming
 */
function heatmapDendrogramViewer() {
    if (typeof heatmapDendrogramViewer.instance === 'object') {
	    return heatmapDendrogramViewer.instance;
    };

    // Initialise subobjects
    var heatmapV =  new heatmapViewer();
    var metaV = new metaDataHeatmapViewer();
    var aspectHeatView = new aspectHeatmapViewer();
    var dendroV =  new dendrogramViewer();

    heatmapDendrogramViewer.instance = this;

    this.initializeComponents();
};

/**
 * Returns the padding that the dendrogram/heatmap/metadata heatmap
 * should be drawn at WITHIN the canvas area.
 */
heatmapDendrogramViewer.prototype.getPlotAreaLeftPadding = function() {
    return p2globalParams.dendrogramHeatmapViewer.paddingLeft;
};

/**
 * Returns the padding that the dendrogram/heatmap/metadata heatmap
 * should be drawn at WITHIN the canvas area.
 */
heatmapDendrogramViewer.prototype.getPlotAreaRightPadding = function() {
    return p2globalParams.dendrogramHeatmapViewer.paddingRight;
};

/**
 * Call the component initializers and do a view update
 */
heatmapDendrogramViewer.prototype.initializeComponents = function() {
  // Initialize heatmap and meta after dendrogram
  var evtBus = new eventBus();
  var dendrogramInitListener = function() {
    var heatView = new heatmapViewer();
    heatView.initialize();

    var metaView = new metaDataHeatmapViewer();
    metaView.initialize();

    var aspectHeatView = new aspectHeatmapViewer();
    aspectHeatView.initialize(function() {
      var eb = new eventBus();
      eb.publish('initialAspectHeatmapLoadComplete');
      
    });

    // Unregister this handler
    var evtBus = new eventBus();
    evtBus.unregister("dendrogram-cell-order-updated", null, dendrogramInitListener);
  };
  evtBus.register("dendrogram-cell-order-updated", null,dendrogramInitListener);

  var dendView = new dendrogramViewer();
  dendView.initialize();
};

/**
 * Update the view of the dendrogram (usually to apply new subsetting)
 * followed by updating the 3 heatmaps that depend on the cell ordering
 * provided by the dendrogram (metadata, aspects and expression heatmap)
 */
heatmapDendrogramViewer.prototype.updateView = function() {
    // Initialize heatmap after dendrogram
    var evtBus = new eventBus();

    // Use a named function so we can remove it from the listeners
    var dendrogramInitListener = function() {
    	var heatView = new heatmapViewer();
    	var metaView = new metaDataHeatmapViewer();
    	var aspectHeatView = new aspectHeatmapViewer();

    	heatView.updateCanvasSize();
    	heatView.drawHeatmap();

    	metaView.updateCanvasSize();
    	metaView.drawMetadata();

    	aspectHeatView.updateCanvasSize();
    	aspectHeatView.drawHeatmap();

    	// Unregister this handler
    	var evtBus = new eventBus();
    	evtBus.unregister("dendrogram-cell-order-updated", null, dendrogramInitListener);
    };

    // Listen for first dendrogram initialization
    evtBus.register("dendrogram-cell-order-updated", null,dendrogramInitListener);

    var dendView = new dendrogramViewer();
    dendView.redrawDendrogram();
};
