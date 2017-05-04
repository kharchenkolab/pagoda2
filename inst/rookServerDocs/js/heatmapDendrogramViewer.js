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

    console.log("Initializing heatmapDendrogramViewer...");

    var heatmapV =  new heatmapViewer();
    var metaV = new metaDataHeatmapViewer();
    var dendroV =  new dendrogramViewer();

    // Are we actually using these? Only in updateView, which will be replaced
    currentCellSelection = [];
    currentGeneSelection = [];

    heatmapDendrogramViewer.instance = this;

    // Set defaults here
   // this.currentLayoutName = p2globalParams.dendrogramHeatmapViewer.defaultLayoutName;


    this.updateContainerSize();

    this.initializeComponents();

    // Keep track of the main container horizontatl size
    var extJsContainer = Ext.getCmp('centreColumnPanel');
    extJsContainer.onResize = function() {
    	var heatDendView = new heatmapDendrogramViewer();
    	heatDendView.updateContainerSize();
    };
};

/**
 * Obtain the size of the container element from ExtJS and store it
 * in the object so that the children can access it
 */
heatmapDendrogramViewer.prototype.updateContainerSize = function() {
    var extJsContainer = Ext.getCmp('centreColumnPanel');
    this.viewerWidth = extJsContainer.body.getWidth(true);
}


/**
 * Returns the name of the current layout
 */
heatmapDendrogramViewer.prototype.getCurrentLayoutName = function(){
    return this.currentLayoutName;
}

/**
 * Updates the name of the layout to use
 */
heatmapDendrogramViewer.prototype.setCurrentLayout = function(layoutName) {
    this.currentLayoutName = layoutName;
}

/**
 * Get the width of all the elements in the dendrogram and heatmap areas
 * @description Provides the width at which the two
 * components should be drawn. It is maintained here to ensure
 * that alignments between the dendrogram and the heatmap are
 * centrally managed.
 * @returns the width of all the elements
 */
heatmapDendrogramViewer.prototype.getCurrentWidth = function() {
    // Just return the width of the parent
    // TODO: Adjust for padding purposes
    return this.viewerWidth;
};

/**
 * Returns the padding that the dendrogram/heatmap/metadata heatmap
 * should be drawn at WITHIN the canvas area.
 */
heatmapDendrogramViewer.prototype.getPlotAreaLeftPadding = function() {
    return p2globalParams.dendrogramHeatmapViewer.paddingLeft;
}

/**
 * Returns the width that the dendrogram/heatmap/metadata heatmap
 * should be drawn at WITHIN the canvas area.
 * @description Returns the width that the dendrogram/heatmap/metadata heatmap
 * should be drawn at WITHIN the canvas area. The size of the canvas
 * areas is returned by a different function of the same obj. This is the same
 * for the dendrogram and heatmap. Does not include labels
 */
heatmapDendrogramViewer.prototype.getPlotAreaWidth = function() {
    return this.viewerWidth - p2globalParams.dendrogramHeatmapViewer.paddingRight;
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
    aspectHeatView.initialize();

    // Unregister this handler
    var evtBus = new eventBus();
    evtBus.unregister("dendrogram-cell-order-updated", null, dendrogramInitListener);
  }
  evtBus.register("dendrogram-cell-order-updated", null,dendrogramInitListener);

  var dendView = new dendrogramViewer();
  dendView.initialize();
}


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
