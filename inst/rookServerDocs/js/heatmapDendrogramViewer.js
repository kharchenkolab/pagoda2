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
    this.currentLayoutName = p2globalParams.dendrogramHeatmapViewer.defaultLayoutName;
 

    this.updateContainerSize();
    
    // Generate the divs and initalise them
    this.generateDrawAreas();
    this.initializeComponents();

    // TODO: Fix me, add listener don't replace
    var extJsContainer = Ext.getCmp('mainViewPanel');
    extJsContainer.onResize = function() {
	var heatDendView = new heatmapDendrogramViewer();
	heatDendView.updateContainerSize();
	heatDendView.updateView();
    };
};

/**
 * Obtain the size of the container element from ExtJS and store it 
 * in the object so that the children can access it
 */
heatmapDendrogramViewer.prototype.updateContainerSize = function() {
    var extJsContainer = Ext.getCmp('mainViewPanel');
    // TODO: move the -30 to some centralised configration 
    this.viewerHeight = extJsContainer.body.getHeight(true) - 30 ;
    this.viewerWidth = extJsContainer.body.getWidth(true);
}

/**
 * Get the element skipping value
 * @param n {number} number of elements to plot
 * @description Get the element skipping value given the  
 * number of elements we are plotting. We keep this in this object
 * so that it exists in a centralised place and the skipping is
 * consistent between different components. Currently not used
 * @todo Improve this
 */
heatmapDendrogramViewer.prototype.getSkipping = function(n) {
    var k =  1000;
    var p = 4 ;

    var skipping;
    if ( n < k) {
	skipping = 1;
    } else {
	skipping = p;
    }
    return skipping;
};

/**
 * Get the current height of the canvas area for the dendrogram.
 * @description get the current height of the canvas area for the dendrogram
 * @returns current canvas height for the dendrogram. This is currently fixed
 */
heatmapDendrogramViewer.prototype.getCurrentDendHeight = function() {
    var layout = this.getCurrentLayoutName();
    return this.viewerHeight * p2globalParams.dendrogramHeatmapViewer.layoutSettings[layout].dendrogramHeight;
};

/**
 * Get the current height of the canvas area used for the heatmap.
 * @description get the current height of the canvas area used for the heatmap
 * @returns Current height of the canvas for the heatmap
 */
heatmapDendrogramViewer.prototype.getCurrentHeatmapHeight = function() {
    var layout = this.getCurrentLayoutName();
    return this.viewerHeight * p2globalParams.dendrogramHeatmapViewer.layoutSettings[layout].heatmapHeight;

};

/**
 * Get current height of the canvas for the metadata heatmap
 * @returns The current metadata heatmap height
 */
heatmapDendrogramViewer.prototype.getCurrentMetadataHeight =  function() {
    var layout = this.getCurrentLayoutName();
    return this.viewerHeight * p2globalParams.dendrogramHeatmapViewer.layoutSettings[layout].metadataHeight;
};

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
 * Generates two canvas elements for plotting the 
 * dendrogram and the heatmap. We are using separate
 * elements because as cell numbers increase the limits 
 * of individual canvas elements will be reached.
 */
heatmapDendrogramViewer.prototype.generateDrawAreas = function() {
    $("#main-window").append(
	'<div id="dendrogram-area-container"></div>' + 
	    '<div id="metadata-area-container"></div>' +
	    '<div id="heatmap-area-container"></div>'
    );
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

	// Unregister this handler
	var evtBus = new eventBus();
	evtBus.unregister("dendrogram-cell-order-updated", null, dendrogramInitListener);
    }
    evtBus.register("dendrogram-cell-order-updated", null,dendrogramInitListener);  

    var dendView = new dendrogramViewer();
    dendView.initialize();

}


/**
 * Do a view update given the current object settings
 */
heatmapDendrogramViewer.prototype.updateView = function() {
    // Initialize heatmap after dendrogram
    var evtBus = new eventBus();

    // Use a named function so we can remove it from the listeners
    var dendrogramInitListener = function() {
	var heatView = new heatmapViewer();
	var metaView = new metaDataHeatmapViewer();

	heatView.updateCanvasSize();
	heatView.drawHeatmap();

	metaView.updateCanvasSize();
	metaView.drawMetadata();
	
	// Unregister this handler
	var evtBus = new eventBus();
	evtBus.unregister("dendrogram-cell-order-updated", null, dendrogramInitListener);
    }

    // Listen for first dendrogram initialization
    evtBus.register("dendrogram-cell-order-updated", null,dendrogramInitListener);  

    var dendView = new dendrogramViewer();
    dendView.redrawDendrogram();
//    dendView.updateOverlay();
};
