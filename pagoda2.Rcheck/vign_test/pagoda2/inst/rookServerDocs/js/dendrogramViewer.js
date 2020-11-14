"use strict";

/*
 * Filename: Dendrogram Viewer
 * Author: Nikolas Barkas
 * Date: March 2017
 */

/**
 * Manages the dendrogram
 * @constructor
 */
function dendrogramViewer() {
    if (typeof dendrogramViewer.instance === 'object') {
        return dendrogramViewer.instance;
    };

    // Click regions
    this.clickRegions = new clickableRegions();

    // The current element width and height
    this.canvasElementWidth = 0;
    this.canvasElementHeight = 0;

    // Initialise the transformation matrix
    // This keeps track of the current canvas transform
    this.currentTransform = [
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1]
    ];

    this.currentHoverHighlight;
    this.currentPermanentHighlight = {
        top: 0,
        left: 0,
        width: 100,
        height: 100
    };

    var extJsContainer = Ext.getCmp('dendrogramPanel');
    extJsContainer.onResize = function() {
        var o = new dendrogramViewer();
        o.updateCanvasSize();
        o.redrawDendrogram();
    };

    this.currentConfiguration = {
        zoomNode: undefined,
        currentSelectedNode: undefined,
        view: "full", // full or zoom
        lineWidth: '2',
        highlightColor: '#FF0000', // hover
        highlightColor2: '#0000FF' // selection
    };

    // This is a hidden canvas for the copy
    this.canvasCopy = document.createElement('canvas');

    // This is a hidden canvas for the second copy
    // Used for the permanent hightlight when something is selected
    this.canvasCopy2 = document.createElement('canvas');

    dendrogramViewer.instance = this;
};

/**
 * Get the currently selected node
 */
dendrogramViewer.prototype.getCurrentSelectedNode = function() {
    return this.currentConfiguration.currentSelectedNode;
}

/**
 * Clear the currently selected node
 */
dendrogramViewer.prototype.clearCurrentSelectedNode = function() {
    this.currentConfiguration.currentSelectedNode = undefined;
}

/**
 * Setup the buttons for the dendrogram control and the  associated
 * listeners.
 */
dendrogramViewer.prototype.initializeButtons = function() {
    var mainPanel = Ext.getCmp('dendrogramPanel');
    var mainPanelHeader = mainPanel.getHeader();

    var aspHeatViewSettingsMenu = new aspectHeatmapViewer().generateAspectHeatmapSettingsMenu();
    var heatViewSettingsMenu = new heatmapViewer().generateHeatmapSettingsMenu();
    var allSettingsMenu = Ext.create("Ext.menu.Menu", {
        id: 'allHeatmapSettingsMenu',
        items: [{
                text: "Aspect Heatmap",
                menu: aspHeatViewSettingsMenu
            },
            {
                text: "Gene Heatmap",
                menu: heatViewSettingsMenu
            }
        ]
    });

    var thisViewer = new dendrogramViewer();
    var toolbar = Ext.create('Ext.Toolbar');
    toolbar.add({
        text: '',
        xtype: 'button',
        tooltip: 'View Parent',
        glyph: 0xf062,
        handler: function() {

            var selCntr = new cellSelectionController();
            var dendView = new dendrogramViewer();

            var selectedNode = dendView.getZoomNode();

            if (typeof selectedNode !== 'undefined') {
                var updateHeatmapOnce = function() {
                    var heatmapV = new heatmapViewer();
                    heatmapV.drawHeatmap();

                    var metaV = new metaDataHeatmapViewer();
                    metaV.drawMetadata();

                    var aspHeatV = new aspectHeatmapViewer();
                    aspHeatV.drawHeatmap();

                    var evtBus = new eventBus();
                    evtBus.unregister("dendrogram-cell-order-updated", null, updateHeatmapOnce);
                }
                var evtBus = new eventBus();
                evtBus.register("dendrogram-cell-order-updated", null, updateHeatmapOnce);

                // Set config to zoom -- for correct redraw
                dendView.currentConfiguration.view = "zoom";

                // Set the zoom node to be the parent of the currently selected zoomed one
                dendView.getInternalNodeParent(dendView.getZoomNode(), function(id) {
                    dendView.setZoomNode(id);

                    // Redraw the dendrogram from the selected node
                    dendView.redrawDendrogram();
                });
            } else {
                Ext.MessageBox.alert('Warning',
                    'Please click on the dendrogram to select a node before zooming in',
                    function() {});
            }
        }
    });

    toolbar.add({
        text: '',
        xtype: 'button',
        tooltip: 'Zoom to current selection',
        glyph: 0xf00e, //fa-search-plus

        handler: function() {
            var selCntr = new cellSelectionController();
            var dendView = new dendrogramViewer();

            var selectedNode = dendView.getCurrentSelectedNode();

            if (typeof selectedNode !== 'undefined') {
                var updateHeatmapOnce = function() {
                    var heatmapV = new heatmapViewer();
                    heatmapV.drawHeatmap();

                    var metaV = new metaDataHeatmapViewer();
                    metaV.drawMetadata();

                    var aspHeatV = new aspectHeatmapViewer();
                    aspHeatV.drawHeatmap();

                    var evtBus = new eventBus();
                    evtBus.unregister("dendrogram-cell-order-updated", null, updateHeatmapOnce);
                }
                var evtBus = new eventBus();
                evtBus.register("dendrogram-cell-order-updated", null, updateHeatmapOnce);

                // Set config to zoom -- for correct redraw
                dendView.currentConfiguration.view = "zoom";

                // Set the zoom node to be the parent of the currently selected one

                // Set the selected node to be the zoom node
                dendView.setZoomNode(selectedNode);

                // Clear the selected node
                dendView.clearCurrentSelectedNode();

                // Clear the permanent highlight
                dendView.clearPermanentHighlightArea();

                // Redraw the dendrogram from the selected node
                dendView.redrawDendrogram();

            } else {
                Ext.MessageBox.alert('Warning', 'Please click on the dendrogram to select a node before zooming in', function() {});
            }

        }
    });

    toolbar.add({
        text: '',
        xtype: 'button',
        tooltip: 'Switch to full view mode',
        glyph: 0xf010, // fa-search-minus
        handler: function() {
            var dendView = new dendrogramViewer();

            if (dendView.currentConfiguration.view !== "full") {

                var updateHeatmapOnce = function() {
                    var heatmapV = new heatmapViewer();
                    heatmapV.drawHeatmap();

                    var metaV = new metaDataHeatmapViewer();
                    metaV.drawMetadata();

                    var aspHeatV = new aspectHeatmapViewer();
                    aspHeatV.drawHeatmap();

                    var evtBus = new eventBus();
                    evtBus.unregister("dendrogram-cell-order-updated", null, updateHeatmapOnce);
                }
                var evtBus = new eventBus();
                evtBus.register("dendrogram-cell-order-updated", null, updateHeatmapOnce);

                // Clear the selected node
                dendView.clearCurrentSelectedNode();

                // Clear the permanent highlight
                dendView.clearPermanentHighlightArea();

                // Redraw the dendrogram from the root
                dendView.currentConfiguration.view = "full";
                dendView.redrawDendrogram();

            } else {
                Ext.MessageBox.alert('Warning', 'You are already viewing the entire dendrogram', function() {});
            };
        }
    });

    toolbar.add({
        text: "",
        type: "button",
        tooltip: 'Download current view',
        glyph: 0xf0ed,
        handler: thisViewer.downloadImagePopUp
    });

    toolbar.add({
        text: "",
        type: "button",
        tooltip: "Show related genes",
        glyph: 0xf002,
        handler: thisViewer.showRelatedGenes
    });

    toolbar.add({
        text: '',
        xtype: 'button',
        tooltip: 'Clear selection overlay',
        glyph: 0xf12d,
        handler: function() {
            var aspHeatView = new aspectHeatmapViewer();
            aspHeatView.clearSelectionOverlay();
            var heatView = new heatmapViewer();
            heatView.clearSelectionOverlay();
            var metaView = new metaDataHeatmapViewer();
            metaView.clearSelectionOverlay();
        }
    });

    toolbar.add({
        text: '',
        xtype: 'button',
        tooltip: 'Configure heatmap plot settings',
        glyph: 0xf013,
        menu: allSettingsMenu
    });

    toolbar.add({
        text: '',
        xtype: 'button',
        tooltip: 'Help',
        glyph: 0xf128,
        handler: function() {
            Ext.create('Ext.window.Window', {
                height: 300,
                width: 400,
                title: 'Help: Dendrogram',
                scrollable: true,
                bodyPadding: 10,
                html: '<h2>Dendrogram </h2>' +
                    '<p>The dendrogram view allows to you see and navigate the relationships between your populations.</p><p>You can click on the dendrogram to select specific portions of your cells. By clicking on a vertical line you will set all the cells below that point as the \'Primary Selection\'. By clicking on a horizontal line you will set the cells on the left subtree below the current position as the \'Primary Selection\' and cells in the left subtree as the \'Secondary selection\'. You can then use the generated selections to perform differential expression between the left and right subtrees at any given position.</p><p>In addition to selecting cells you can use the dendrogram to zoom in specific subpopulations in your data. After making a selection you can click on the <span style="font-family: FontAwesome">&#xf00e</span> (Zoom) button to only display these cells on all the heatmaps. When you are in zoom mode you can zoom back to the original view by clicking on the <span style="font-family: FontAwesome">&#xf010</span> (Full View) button. Alternatively you can zoom out one level at a time by clicking on the <span style="font-family: FontAwesome">&#xf063</span> (Parent) button. Finally, you can download the current view by clicking on the <span style="font-family: FontAwesome">&#xf0ed</span> (Download) button.</p>' +
                    '<h2>Metadata heatmap</h2>' +
                    '<p>The heatmap displays metadata information about the cells, such as batch of origin and sequencing depth.</p>' +
                    '<p>Single click to identify individuals cell and corresponding metadata entry. The information can be seen in the status bar at the bottom left. Double click to color the embedding by the metadata row under your cursor. You can right-click on a cell for more options. This will allow you to select all the cells that belong to the same cluster as the given cell. The new cell selection will appear in the cell selections panel with the name you specify</p>' +
                    '<p>You can download the current view using the <span style="font-family: FontAwesome">&#xf0ed</span> (download) icon. In some cases the downloaded file will not have the correct extension, please rename it to end in ".png" if that happens. You can clear highlighting of cells using the <span style="font-family: FontAwesome">&#xf12d</span> (clear) icon.</p>' +
                    '<h2>Aspect heatmap</h2>' +
                    '<p>The heatmap displays cell weight values for the aspects.' +
                    ' Aspects are shown as rows and cells as columns</p>' +
                    '<p>Double click to color the embedding by the selected aspect.' +
                    'You can hover with your mouse to identify genes and see the correspondence ' +
                    ' of the underlying cell in other heatmaps.</p>' +
                    '<p>You can download the current view using the <span style="font-family: FontAwesome">&#xf0ed</span> (download) icon. In some cases the downloaded file will not have the correct extension, please rename it to end in ".png" if that happens. You can clear highlighting of cells using the <span style="font-family: FontAwesome">&#xf12d</span> (clear) icon. Finally, you can use the <span style="font-family: FontAwesome">&#xf013</span> (settings) icon to adjust the view of the heatmap. Specifically you can adjust the color palette used for plotting and the number of colours available. Some palettes only support a limited range or a fixed number of colours and this may limit the number of values you can enter.</p>' +
                    '<h2>Main heatmap</h2>' +
                    '<p>The heatmap displays expression values for the selected genes.' +
                    ' Genes are shown as rows and cells as columns</p>' +
                    '<p>Double click to color the embedding by the selected gene expression.' +
                    'You can hover with your mouse to identify genes and see the correspondence ' +
                    ' of the underlying cell in other heatmaps.</p>' +
                    '<p>You can download the current view using the <span style="font-family: FontAwesome">&#xf0ed</span> (download) icon. In some cases the downloaded file will not have the correct extension, please rename it to end in ".png" if that happens. You can clear highlighting of cells using the <span style="font-family: FontAwesome">&#xf12d</span> (clear) icon. Finally, you can use the <span style="font-family: FontAwesome">&#xf013</span> (settings) icon to adjust the view of the heatmap. Specifically you can adjust the color palette used for plotting and the number of colours available. Some palettes only support a limited range or a fixed number of colours and this may limit the number of values you can enter. The reorder rows option (default: on) enabled reordering of the gene rows. When the checkbox is selected the genes will appear in the order that they appear in the table used to select them. When the checkbox is selected the genes will be reordered on the fly. Reordering is performed by kmeans clustering of the rows.</p>',
                constrain: true,
                closable: true,
                resizable: false
            }).show();
        } // handler
    }); // toolbar add

    mainPanelHeader.add(toolbar);
};

/**
 * Resolve a click
 * @description Find which element was clicked and call the callback function
 * with information about it
 * @private
 */
dendrogramViewer.prototype.resolveClick = function(x, y, callback) {
    var localCoord = this.transformToLocalCoordinates(x, y);
    var x = localCoord[0];
    var y = localCoord[1];

    this.clickRegions.resolveClick(x, y, callback);
};

/**
 * Resolve a click synchronously
 * @description Find which element was clicked and call the callback function
 * with information about it
 * @private
 */
dendrogramViewer.prototype.resolveClickSync = function(x, y) {
    var localCoord = this.transformToLocalCoordinates(x, y);
    var x = localCoord[0];
    var y = localCoord[1];

    return (this.clickRegions.resolveClick(x, y));
};


/**
 * Get the two children of the specified node and return them in the
 * callback function
 * @param data the data from the data controller getReducedDendrogram() call
 * @param nodeId the id to look up the children of
 */
dendrogramViewer.prototype.getNodeImmediateChildren = function(data, nodeId) {
    var n = data.order.length;
    var mergeArray = pagHelpers.serialisedArrayTo2D(data["merge"], n - 1, 2);
    return mergeArray[nodeId - 1];
};

/**
 * Get the parent node of the specified internal node
 * @nodeId the internal node id, 1 indexed
 */
dendrogramViewer.prototype.getInternalNodeParent = function(nodeId, callback) {
    var dataCntr = new dataController();
    dataCntr.getReducedDendrogram(function(data) {
        var retValue = undefined;

        var n = data.order.length;
        var mergeArray = pagHelpers.serialisedArrayTo2D(data["merge"], n - 1, 2);

        if (nodeId >= n - 1) {
            // We are at the head node
        } else {
            for (var i = 0; i < n; i++) {
                if (mergeArray[i][0] == nodeId || mergeArray[i][1] == nodeId) {
                    retValue = i + 1;
                    break; // Stop searching
                };
            };
            callback(retValue);
        }
    });
}

/**
 * Get an object with all the leafs under this node
 * @param data The hclust data
 * @param nodeId The nodeId the terminal leafs of which to get, 1-indexed as in the merge matrix
 */
dendrogramViewer.prototype.getNodeDescendents = function(data, nodeId) {
    var n = data.order.length;
    var mergeArray = pagHelpers.serialisedArrayTo2D(data["merge"], n - 1, 2);

    var pendingVisit = [];
    var children = [];

    // Starting point
    pendingVisit.push(nodeId);

    while (pendingVisit.length > 0) {
        var currentNode = pendingVisit.pop();
        if (currentNode < 0) {
            // It's a leaf
            // Add 1 so that we don't change the element s
            // Was was subtracted when they were pushed
            children.push(Math.abs(currentNode));
        } else {
            // It's an internal node visit its children
            // Subtract 1 because the numbers in the merge array
            // Refer to 1-indexes R positions
            pendingVisit.push(mergeArray[currentNode - 1][0]);
            pendingVisit.push(mergeArray[currentNode - 1][1]);
        }
    }
    return children;
}

/**
 * Updates the transform matrix using the built in
 * transform function and keeps track of the
 * applied transformations
 */
dendrogramViewer.prototype.transform = function(a, b, c, d, e, f) {
    //dendArea = $('#dendrogram-area')[0];
    //var ctx = dendArea.getContext("2d");
    // Read here for details
    // https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Transformations

    // Update the locally kept transform matrix
    var dendV = new dendrogramViewer();
    var newTransformMatrix = [
        [a, c, e],
        [b, d, f],
        [0, 0, 1]
    ];
    // Multiplication is done the other way around to
    // keep in line with the orientation of the matrices
    // that canvas maintains
    dendV.currentTransform = (math.multiply(newTransformMatrix, dendV.currentTransform));
    dendV.restoreCurrentTransform();
};

/**
 * Sets the specified transformation matrix and keeps track of it
 */
dendrogramViewer.prototype.setTransform = function(a, b, c, d, e, f) {
    // Update the locally kept transform matrix
    var dendV = new dendrogramViewer();
    var newTransformMatrix = [
        [a, c, e],
        [b, d, f],
        [0, 0, 1]
    ];

    // Multiplication is done the other way around to
    // keep in line with the orientation of the matrices
    // that canvas maintains
    dendV.currentTransform = newTransformMatrix;
    dendV.restoreCurrentTransform();
};

/**
 * Returns current transformation
 */
dendrogramViewer.prototype.getCurrentTransform = function() {
    var dendV = new dendrogramViewer();
    return dendV.currentTransform;
};

/**
 * Transforms an x and y from the canvas back to the plot coordinates
 * @description Takes an x and y from the canvas and
 * takes it back through the applied transformation to
 * find which position in the canvas plotting coordinate
 * frame was clicked
 * @param x the x-coordinate
 * @param u the y-coordinate
 */
dendrogramViewer.prototype.transformToLocalCoordinates = function(x, y) {
    var dendV = new dendrogramViewer();

    // Augment the point
    var p = [x, y, 1];
    var invTr = dendV.getCurrentInverseTransform();

    // Map back through current transform
    var np = math.multiply(invTr, math.transpose(p));

    // Return x and y only
    return np.slice(0, 2);
};

/**
 * Transform to canvas plot to regular canvas coordinates
 */
dendrogramViewer.prototype.transformToCanvasCoordinates = function(x, y) {
    var p = [x, y, 1];
    var tr = this.getCurrentTransform();
    var np = math.multiply(tr, math.transpose(p));
    return np.slice(0, 2);
}

/**
 * Put the current transform that we have in our matrix
 * into the canvas object
 */
dendrogramViewer.prototype.restoreCurrentTransform = function() {
    var dendV = new dendrogramViewer();
    var curT = dendV.currentTransform;
    var a = curT[0][0];
    var b = curT[2][0];
    var c = curT[0][1];
    var d = curT[1][1];
    var e = curT[0][2];
    var f = curT[1][2];

    // Transform main canvas
    var dendArea = $('#dendrogram-area')[0];
    var ctx = dendArea.getContext("2d");
    ctx.setTransform(a, b, c, d, e, f);

    // Transform the overlay canvas as well
    var dendAreaOverlay = $('#dendrogram-area-overlay')[0];
    var ctx2 = dendAreaOverlay.getContext("2d");
    ctx2.setTransform(a, b, c, d, e, f);

    // Transform the internal copy
    var ctx3 = this.canvasCopy.getContext('2d');
    ctx3.setTransform(a, b, c, d, e, f);

    var ctx4 = this.canvasCopy2.getContext('2d');
    ctx4.setTransform(a, b, c, d, e, f);
}


/**
 * Get the inverse of the current transformation
 * If calls to this become common, save as a obj variable
 * during set time
 */
dendrogramViewer.prototype.getCurrentInverseTransform = function() {
    var dendV = new dendrogramViewer();
    return math.inv(dendV.currentTransform);
}

/**
 * This function will draw the dendrogram with a root at
 * topnode. If no topnode is provided it will draw the entire dendrogram
 * A node identifier is the merge matrix position that generated this node
 * @param topnode the topmost node to draw, 1-indexed
 */
dendrogramViewer.prototype.drawDendrogram = function(topnode) {
    this.updateCanvasSize();



    var dataCntr = new dataController();
    dataCntr.getReducedDendrogram(function(data) {


        var dendViewer = new dendrogramViewer();

        var rootNodeId = 0;
        if (typeof topnode === 'undefined') {
            rootNodeId = data.merge.length / 2;
            // Set config
            dendViewer.currentConfiguration.show = "all";
            dendViewer.currentConfiguration.topnode = null;

        } else {
            rootNodeId = topnode;
            // Set config
            dendViewer.currentConfiguration.show = "topnode";
            dendViewer.currentConfiguration.topnode = topnode;
        }

        dendViewer.drawDendrogramSubsetWithData(data, rootNodeId);
    });
};

/**
 * Given the order of the clusters and the number of items
 * they contain return an array with the start and end positions
 * of the clusters as they are ploted.
 * @param data the data from the server side
 */
dendrogramViewer.prototype.getClusterPositionRanges = function(data) {
    // TODO: this can in principle be cached

    var memCountInOrder = [];

    // Put them in order (start pos depends on what comes before)
    for (var i = 0; i < data.order.length; i++) {
        memCountInOrder[i] = data.clusterMemberCount[data.order[i]];
    }

    // Calculate the coordinates
    var coordInOrder = [];
    var lastPos = 0;
    for (var i = 0; i < memCountInOrder.length; i++) {
        var start = lastPos + 1;
        var end = start + memCountInOrder[i] - 1;
        coordInOrder[i] = [start, end];
        lastPos = end;
    }

    // Put the coordinates back in order
    var coordOfNodes = [];
    for (var i = 1; i <= data.order.length; i++) {
        var indx = data.order.indexOf(i);
        coordOfNodes[i] = coordInOrder[indx];
    }

    return coordOfNodes;
}

/**
 * Update the internally kept display cell order
 */
dendrogramViewer.prototype.updateCurrentDisplayCells = function(callback) {
    var dataCntr = new dataController();
    var dendV = this;
    dataCntr.getCellOrder(function(cellnames) {
        var range = dendV.getCurrentDisplayCellsIndexes();
        dendV.currentConfiguration.currentDisplayCells = cellnames.slice(range[0], range[1]);
        // Callback is just for notification here
        callback();
    });
}

/**
 * Return the ids of the current display cells in order
 */
dendrogramViewer.prototype.getCurrentDisplayCells = function(callback) {
    return this.currentConfiguration.currentDisplayCells;
}

/**
 * Return the cell indexes that are to be plotted
 */
dendrogramViewer.prototype.getCurrentDisplayCellsIndexes = function() {
    -p2globalParams.dendrogramHeatmapViewer.paddingRight
    return [this.currentConfiguration.startCellIndex,
        this.currentConfiguration.endCellIndex
    ];
}

/**
 * Get the current height of the region
 */
dendrogramViewer.prototype.getHeight = function() {
    return Ext.getCmp('dendrogramPanel').getHeight() - 45;
}

/**
 * Get the current width of the region
 */
dendrogramViewer.prototype.getWidth = function() {
    return (Ext.getCmp('dendrogramPanel').getWidth());
}

/**
 * Draw a dendrogram that only includes everything under the specified node
 * @param data the data as received from the dataController
 * @param topnode the identifier for the top most node to draw
 */
dendrogramViewer.prototype.drawDendrogramSubsetWithData = function(data, topnode) {
    // Get the dendrogram viewer object
    var dendV = new dendrogramViewer();
    var heatDendView = new heatmapDendrogramViewer();

    // These are the leaf node ids that are below our topnode
    var children = this.getNodeDescendents(data, topnode);

    // Recover the data
    var n = data.order.length;
    var mergeArray = pagHelpers.serialisedArrayTo2D(data["merge"], n - 1, 2);

    // Max height is  the height of the top node
    var maxHeight = data.height[topnode - 1];

    // Vertical scaling
    var vscale = this.getHeight() / maxHeight * p2globalParams.dendrogram.scaleFactor;

    // Get the canvas object
    var dendAreaOuter = $('#dendrogram-area');
    var dendArea = dendAreaOuter[0];
    var ctx = dendArea.getContext("2d");
    ctx.save();

    // Set line width
    ctx.lineWidth = this.currentConfiguration.lineWidth;

    // Clear the canvas
    ctx.clearRect(0, 0, this.getWidth(), this.getHeight())

    // Calculating leaf positions
    var coordOfNodes = dendV.getClusterPositionRanges(data);

    // Get the mid-points of the  clusters
    var nodePositions = [];
    for (var i = 1; i < coordOfNodes.length; i++) {
        nodePositions[i] = [(coordOfNodes[i][0] + coordOfNodes[i][1]) / 2, 0];
    }

    // Get Min/max only for the children of the nodes we are plotting
    var scaleMin = +Infinity;
    var scaleMax = -Infinity;
    for (var i = 1; i < coordOfNodes.length; i++) {
        if (children.indexOf(i) >= 0) {
            if (coordOfNodes[i][0] < scaleMin) {
                scaleMin = coordOfNodes[i][0];
            }
            if (coordOfNodes[i][1] > scaleMax) {
                scaleMax = coordOfNodes[i][1];
            }
        }
    }

    // Update the configuration of the dendrogram with
    dendV.currentConfiguration.startCellIndex = scaleMin - 1;
    dendV.currentConfiguration.endCellIndex = scaleMax - 1;

    // This updates the current cell order (by identifier) and raises the event.
    dendV.updateCurrentDisplayCells(function() {
        var evtBus = new eventBus();
        evtBus.publish('dendrogram-cell-order-updated');
    });

    // Calculate the scallings for the transformation
    var heatDendV = new heatmapDendrogramViewer();
    var horizontalScaling = (this.getWidth() - heatDendV.getPlotAreaRightPadding()) / (scaleMax - scaleMin + 1);
    var horizontalShift = -1 * scaleMin * horizontalScaling +
        heatDendView.getPlotAreaLeftPadding();


    // Two helper function for doing the transformation before plotting
    // instead of using the canvas.
    function scaleX(x) {
        return x * horizontalScaling + horizontalShift;
    }

    function scaleY(y) {
        return y * vscale;
    }

    // Set cartesian coordinates on the canvas
    dendV.setTransform(1, 0, 0, -1, 0, dendArea.height);

    // Clear the current Click Areas
    dendV.clickRegions.clearClickAreas();

    // An array that holds the internal positions in [x,y] format
    // This is needed so that when a merge refers to a previous
    // merge the plot start can be easily determined
    var internalNodePositions = [];

    // This array holds [x1, x2] range of x-coordinates that
    // are to be highlighted for each internal node
    var internalNodeOverlayRanges = [];

    // Loop over merge array
    for (var i = 0; i < mergeArray.length; i++) {
        // Keeping track of node boundaries
        var currentNodeLeft = +Infinity;
        var currentNodeRight = -Infinity;

        // Leaf or internal
        var nodeType = null;

        // Height for this step
        var drawHeight = data.height[i];

        // Merge array entry
        var mergeStep = mergeArray[i];

        // The two points to connect with a horisontal line in the last step
        var p1 = undefined;
        var p2 = undefined;

        // Step 1:
        // draw line for node1 to appropriate height
        // register this line with the click array
        if (mergeStep[0] < 0) {
            // A leaf node (-ve), check if it is in the children array
            var nodeId = Math.abs(mergeStep[0]);
            nodeType = "leaf";
            if (children.indexOf(nodeId) >= 0) {
                // Keep track of left and right for this node
                currentNodeLeft = Math.min(currentNodeLeft, Math.min(coordOfNodes[nodeId][0],
                    coordOfNodes[nodeId][1]));
                currentNodeRight = Math.max(currentNodeRight, Math.max(coordOfNodes[nodeId][0],
                    coordOfNodes[nodeId][1]));

                p1 = nodePositions[nodeId];

                ctx.beginPath();
                ctx.moveTo(scaleX(p1[0]), scaleY(0));
                ctx.lineTo(scaleX(p1[0]), scaleY(drawHeight));
                ctx.stroke();

                // No click region for nodes
            }
        } else {
            // An internal node
            var nodeId = mergeStep[0];
            nodeType = "internal";
            var intNodePos = internalNodePositions[nodeId];

            // It is possible that the internal node does not
            // exist because its descendents are not part of
            // what we are currently plotting
            if (typeof intNodePos !== 'undefined') {

                currentNodeLeft = Math.min(currentNodeLeft, Math.min(internalNodeOverlayRanges[nodeId][0],
                    internalNodeOverlayRanges[nodeId][1]));
                currentNodeRight = Math.max(currentNodeRight, Math.max(internalNodeOverlayRanges[nodeId][0],
                    internalNodeOverlayRanges[nodeId][1]));

                // Draw vertical line
                ctx.beginPath();
                ctx.moveTo(scaleX(intNodePos[0]), scaleY(intNodePos[1]));
                ctx.lineTo(scaleX(intNodePos[0]), scaleY(drawHeight));
                ctx.stroke();

                var x1 = scaleX(intNodePos[0]) - 2;
                var y1 = scaleY(intNodePos[1]) - 2;
                var x2 = scaleX(intNodePos[0]) + 2;
                var y2 = scaleY(drawHeight) + 2;

                var left = scaleX(Math.min(internalNodeOverlayRanges[nodeId][0], internalNodeOverlayRanges[nodeId][1]));
                var width = scaleX(Math.max(internalNodeOverlayRanges[nodeId][0], internalNodeOverlayRanges[nodeId][1])) - left;

                dendV.clickRegions.addClickArea(
                    x1, y1,
                    x1, y2,
                    x2, y2,
                    x2, y1, {
                        nodeId: nodeId,
                        nodeType: 'vertical',
                        highlightBoundingbox: {
                            top: 0,
                            left: left,
                            width: width,
                            height: scaleY(drawHeight) - this.currentConfiguration.lineWidth * 2
                        }
                    }
                );


                p1 = intNodePos;
            }
        }

        // Step 2:
        // draw line for node2 to appropriate height
        // register this line with the click array
        if (mergeStep[1] < 0) {
            var nodeId = Math.abs(mergeStep[1]);
            nodeType = "leaf";
            if (children.indexOf(nodeId) >= 0) {
                p2 = nodePositions[nodeId];

                // Keep track of left and right for this node
                currentNodeLeft = Math.min(currentNodeLeft, Math.min(coordOfNodes[nodeId][0],
                    coordOfNodes[nodeId][1]));
                currentNodeRight = Math.max(currentNodeRight, Math.max(coordOfNodes[nodeId][0],
                    coordOfNodes[nodeId][1]));

                ctx.beginPath();
                ctx.moveTo(scaleX(p2[0]), scaleY(0));
                ctx.lineTo(scaleX(p2[0]), scaleY(drawHeight));
                ctx.stroke();
            }
        } else {
            // An internal node
            var nodeId = mergeStep[1];
            nodeType = "internal";
            var intNodePos = internalNodePositions[nodeId];

            if (typeof intNodePos !== 'undefined') {
                currentNodeLeft = Math.min(currentNodeLeft, Math.min(internalNodeOverlayRanges[nodeId][0],
                    internalNodeOverlayRanges[nodeId][1]));
                currentNodeRight = Math.max(currentNodeRight, Math.max(internalNodeOverlayRanges[nodeId][0],
                    internalNodeOverlayRanges[nodeId][1]));

                // Draw vertical line
                ctx.beginPath();
                ctx.moveTo(scaleX(intNodePos[0]), scaleY(intNodePos[1]));
                ctx.lineTo(scaleX(intNodePos[0]), scaleY(drawHeight));
                ctx.stroke();

                var x1 = scaleX(intNodePos[0]) - 2;
                var y1 = scaleY(intNodePos[1]) - 2;
                var x2 = scaleX(intNodePos[0]) + 2;
                var y2 = scaleY(drawHeight) + 2;



                var left = scaleX(Math.min(internalNodeOverlayRanges[nodeId][0], internalNodeOverlayRanges[nodeId][1]));
                var width = scaleX(Math.max(internalNodeOverlayRanges[nodeId][0], internalNodeOverlayRanges[nodeId][1])) - left;


                dendV.clickRegions.addClickArea(
                    x1, y1,
                    x1, y2,
                    x2, y2,
                    x2, y1, {
                        nodeId: nodeId,
                        nodeType: 'vertical',
                        highlightBoundingbox: {
                            top: 0,
                            left: left,
                            width: width,
                            height: scaleY(drawHeight) - this.currentConfiguration.lineWidth * 2
                        }
                    }
                );

                p2 = intNodePos;
            }
        }

        // Step 3:
        // connect the two segments with an horizontal line
        // registter this line with the click array
        // record the position of the internal node

        // Draw the connecting horisontal segment
        if (!((typeof(p1) === 'undefined') | (typeof(p2) === 'undefined'))) {
            ctx.beginPath();
            ctx.moveTo(scaleX(p1[0]), scaleY(drawHeight));
            ctx.lineTo(scaleX(p2[0]), scaleY(drawHeight));
            ctx.stroke();

            var x1 = scaleX(p1[0]) - 2;
            var y1 = scaleY(drawHeight) - 2;
            var x2 = scaleX(p2[0]) + 2;
            var y2 = scaleY(drawHeight) + 2;

            dendV.clickRegions.addClickArea(
                x1, y1,
                x1, y2,
                x2, y2,
                x2, y1, {
                    nodeId: i + 1,
                    nodeType: 'horizontal',
                    highlightBoundingbox: {
                        top: scaleY(0),
                        left: scaleX(currentNodeLeft),
                        width: scaleX(currentNodeRight) - scaleX(currentNodeLeft),
                        height: scaleY(drawHeight) + dendV.currentConfiguration.lineWidth * 2
                    }
                }
            );

            // Keep track of the internal node position
            internalNodePositions[i + 1] = ([((p1[0] + p2[0]) / 2), drawHeight]);

            // Keep track of node leaf boundaries
            internalNodeOverlayRanges[i + 1] = [currentNodeLeft, currentNodeRight];
        }
    }; // end loop over mergeArray


    ctx.restore();

    this.updateCanvasCopy();
} // dendrogramViewer.prototype.drawDendrogramSubsetWithData

/**
 * Update the internal canvas copy object we use for the
 * overlay at hover.
 */
dendrogramViewer.prototype.updateCanvasCopy = function() {
    var mainCanvas = document.getElementById('dendrogram-area');

    var ctx = this.canvasCopy.getContext('2d');

    // Save draw settings
    ctx.save();

    // Copy the canvas over
    ctx.drawImage(mainCanvas, 0, 0, this.canvasElementWidth, this.canvasElementHeight,
        0, 0, this.canvasElementWidth, this.canvasElementHeight);

    // Change the color
    ctx.beginPath();
    ctx.fillStyle = this.currentConfiguration.highlightColor;
    ctx.globalCompositeOperation = 'source-in';
    ctx.fillRect(0, 0, this.canvasElementWidth, this.canvasElementHeight);

    // Restore draw settings
    ctx.restore();


    // Do the same for the second canvas
    var ctx2 = this.canvasCopy2.getContext('2d');
    ctx2.save();
    ctx2.drawImage(mainCanvas, 0, 0, this.canvasElementWidth, this.canvasElementHeight,
        0, 0, this.canvasElementWidth, this.canvasElementHeight);
    ctx2.beginPath();
    ctx2.fillStyle = this.currentConfiguration.highlightColor2;
    ctx2.globalCompositeOperation = 'source-in';
    ctx2.fillRect(0, 0, this.canvasElementWidth, this.canvasElementHeight);
    ctx2.restore();
}

/**
 * Set the current zoom node
 */
dendrogramViewer.prototype.setZoomNode = function(nodeId) {
    this.currentConfiguration.zoomNode = nodeId;
}

/**
 * Get the current zoom node
 */
dendrogramViewer.prototype.getZoomNode = function() {
    return this.currentConfiguration.zoomNode;
}

/**
 * Get information about what the canvas size should be from
 * the heatmapDendrogramViewer
 */
dendrogramViewer.prototype.updateCanvasSize = function() {
    // Get current dimentions
    var heatDendV = new heatmapDendrogramViewer();
    var curWidth = this.getWidth();

    var curHeight = this.getHeight();

    // Keep track within the object
    this.canvasElementWidth = curWidth;
    this.canvasElementHeight = curHeight;

    // Update the main canvas
    var dendArea = $('#dendrogram-area')[0];
    dendArea.width = curWidth;
    dendArea.height = curHeight;

    // Update the overlay canvas
    var dendAreaOverlay = $('#dendrogram-area-overlay')[0];
    dendAreaOverlay.width = curWidth;
    dendAreaOverlay.height = curHeight;

    // Update the hidden copy canvas (used for copying to overlay)
    this.canvasCopy.width = curWidth;
    this.canvasCopy.height = curHeight;

    // Update the second copy
    this.canvasCopy2.width = curWidth;
    this.canvasCopy2.height = curHeight;

    // Update the container
    var dendAreaContainer = $('#dendrogram-area-container');
    dendAreaContainer.css({
        width: curWidth + 'px',
        height: curHeight + 'px'
    });
}


/**
 * Setup the DOM elements required for the dendrogramViewer
 */
dendrogramViewer.prototype.setupDOM = function() {
    // The setup is normal outer, relative inner and absolute canvases
    // both at 0,0

    var dendrogramContainer = $('#dendrogram-area-container');
    dendrogramContainer.append('<div id="dendrogram-area-container-inner"></div>');

    var dendrogramContainerInner = $('#dendrogram-area-container-inner');
    dendrogramContainerInner.css({
        position: 'relative'
    });

    dendrogramContainerInner.append('<canvas id="dendrogram-area"></canvas>' +
        '<canvas id="dendrogram-area-overlay"></canvas>');

    var dendrogramArea = $('#dendrogram-area');
    dendrogramArea.css({
        position: 'absolute',
        top: 0,
        left: 0
    });

    var dendrogramAreaOverlay = $('#dendrogram-area-overlay');
    dendrogramAreaOverlay.css({
        position: 'absolute',
        top: 0,
        left: 0
    });
}

/**
 * Updates the overlay
 */
dendrogramViewer.prototype.updateOverlay = function() {
    var dendV = new dendrogramViewer();

    var ctx = document.getElementById('dendrogram-area-overlay').getContext('2d');
    ctx.save();

    // Clear the overlay
    ctx.clearRect(0, 0, dendV.canvasElementWidth, dendV.canvasElementHeight);

    // Permanent highlight
    var permanentHighlight = this.getPermanentHighlightArea();
    if (typeof permanentHighlight !== 'undefined') {
        ctx.drawImage(this.canvasCopy2,
            permanentHighlight.left, permanentHighlight.top,
            permanentHighlight.width, permanentHighlight.height,
            permanentHighlight.left, permanentHighlight.top,
            permanentHighlight.width, permanentHighlight.height);
    }

    // Rollover hightlight
    var boundingBox = this.currentHoverHighlight;
    if (typeof boundingBox !== 'undefined') {
        var cpwidth = boundingBox.width;
        var cpheight = boundingBox.height;
        var top = boundingBox.top;
        var left = boundingBox.left;

        // This is the rollover highlight
        ctx.drawImage(this.canvasCopy,
            left, top, cpwidth, cpheight,
            left, top, cpwidth, cpheight);
    }

    ctx.restore();
}

/**
 * Setup event listeners for the dendrogram
 * @private
 */
dendrogramViewer.prototype.setupListeners = function() {

    // Mouse move for node hightlighting
    $('#dendrogram-area-overlay').mousemove(function(e) {
        var x = e.originalEvent.offsetX;
        var y = e.originalEvent.offsetY;

        var dendV = new dendrogramViewer();

        var res = dendV.resolveClickSync(x, y);
        if (typeof res !== 'undefined') {
            if (typeof res.highlightBoundingbox !== 'undefined') {
                // This area has a highlight box associated with it
                dendV.highlightArea(res.highlightBoundingbox);
                dendV.updateOverlay();
            }
        } else {
            // We  are not on an overlay
            dendV.clearHighlight();
            dendV.updateOverlay();
        }
    });

    // Clear the hover canvas when the mouse leaves the area
    $('#dendrogram-area-overlay').mouseout(function(e) {
        var dendV = new dendrogramViewer;
        dendV.clearHighlight();
    });

    // Click listener
    $('#dendrogram-area-overlay').click(function(e) {
        // TODO: This is probably not the best way to get the coordinates
        var x = e.originalEvent.offsetX;
        var y = e.originalEvent.offsetY;

        var dendV = new dendrogramViewer();

        dendV.resolveClick(x, y, function(clickData) {
            var nodeId = clickData.nodeId;
            var type = clickData.nodeType;

            // Highlight area for selected node
            dendV.setPermanentHighlightArea(clickData.highlightBoundingbox);

            // Save the currently selected node
            dendV.currentConfiguration.currentSelectedNode = nodeId;

            // Update the current cell selection depending on the type
            // of the node we clicked
            if (type === "vertical") {
                // Vertical node
                var dataCntr = new dataController();
                dataCntr.getReducedDendrogram(function(data) {
                    var descendants = dendV.getNodeDescendents(data, nodeId);

                    // Get the start end positions of the clusters
                    var clusterPositions = dendV.getClusterPositionRanges(data);

                    var selectedCellIdentifiers = new Array();
                    dataCntr.getCellOrder(function(cellorder) {
                        for (var i = 0; i < descendants.length; i++) {
                            var currDesc = descendants[i];
                            var currPos = clusterPositions[currDesc];
                            var cellsInCluster = cellorder.slice(currPos[0] - 1, currPos[1] - 1);

                            selectedCellIdentifiers.push.apply(selectedCellIdentifiers, cellsInCluster);
                        }

                        var selectionMetadata = {
                            clusters: descendants
                        };

                        // Set the cell selection
                        var cellSelCntr = new cellSelectionController();
                        cellSelCntr.setSelection(selectedCellIdentifiers,
                            'Primary Selection', selectionMetadata, "#FF0000", 'currentPrimarySel');
                    }); // getCellOrder();
                }); // getReducedDendrogram();
            } else if (type == "horizontal") {
                var dataCntr = new dataController();
                dataCntr.getReducedDendrogram(function(data) {
                    var immediateChildren = dendV.getNodeImmediateChildren(data, nodeId);

                    // Process the left child
                    if (immediateChildren[0] < 0) {
                        // Just a single cluster

                        dataCntr.getCellOrder(function(cellorder) {
                            var clusterPositions = dendV.getClusterPositionRanges(data);

                            var currDesc = Math.abs(immediateChildren[0]);
                            var currPos = clusterPositions[currDesc];
                            var cellsInCluster = cellorder.slice(currPos[0] - 1, currPos[1] - 1);

                            var selectionMetadata = {
                                clusters: [currDesc]
                            }

                            var cellSelCntr = new cellSelectionController();
                            cellSelCntr.setSelection(cellsInCluster,
                                'Primary Selection', selectionMetadata, "#FF0000", 'currentPrimarySel');
                        });
                    } else {
                        // The left child is an internal node

                        var descendants = dendV.getNodeDescendents(data, immediateChildren[0]);

                        // Get the start end positions of the clusters
                        var clusterPositions = dendV.getClusterPositionRanges(data);

                        var selectedCellIdentifiers = new Array();
                        dataCntr.getCellOrder(function(cellorder) {
                            for (var i = 0; i < descendants.length; i++) {
                                var currDesc = descendants[i];
                                var currPos = clusterPositions[currDesc];
                                var cellsInCluster = cellorder.slice(currPos[0] - 1, currPos[1] - 1);

                                selectedCellIdentifiers.push.apply(selectedCellIdentifiers, cellsInCluster);
                            }

                            var selectionMetadata = {
                                clusters: descendants
                            };

                            // Set the cell selection
                            var cellSelCntr = new cellSelectionController();
                            cellSelCntr.setSelection(selectedCellIdentifiers,
                                'Primary Selection', selectionMetadata, "#FF0000", 'currentPrimarySel');
                        }); // getCellOrder();
                    } // else

                    // Process the right child
                    if (immediateChildren[1] < 0) {
                        // Just a single cluster

                        dataCntr.getCellOrder(function(cellorder) {
                            var clusterPositions = dendV.getClusterPositionRanges(data);

                            var currDesc = Math.abs(immediateChildren[1]);
                            var currPos = clusterPositions[currDesc];
                            var cellsInCluster = cellorder.slice(currPos[0] - 1, currPos[1] - 1);

                            var selectionMetadata = {
                                clusters: [currDesc]
                            }

                            var cellSelCntr = new cellSelectionController();
                            cellSelCntr.setSelection(cellsInCluster,
                                'Secondary selection', selectionMetadata, "#FF0000", 'currentSecondarySel');
                        });
                    } else {
                        descendants = dendV.getNodeDescendents(data, immediateChildren[1]);

                        // Get the start end positions of the clusters
                        var clusterPositions = dendV.getClusterPositionRanges(data);

                        var selectedCellIdentifiers = new Array();
                        dataCntr.getCellOrder(function(cellorder) {
                            for (var i = 0; i < descendants.length; i++) {
                                var currDesc = descendants[i];
                                var currPos = clusterPositions[currDesc];
                                var cellsInCluster = cellorder.slice(currPos[0] - 1, currPos[1] - 1);

                                selectedCellIdentifiers.push.apply(selectedCellIdentifiers, cellsInCluster);
                            }

                            var selectionMetadata = {
                                clusters: descendants
                            };

                            // Set the cell selection
                            var cellSelCntr = new cellSelectionController();
                            cellSelCntr.setSelection(selectedCellIdentifiers,
                                'Secondary Selection', selectionMetadata, "#FF0000", 'currentSecondarySel');
                        }); // getCellOrder();
                    } // else


                }); // getReducedDendrogram
            } // if type horizontal
        }); // dendV.resolveClick
    }); //   $('#dendrogram-area').click(function(e) -- end of callback
}

/**
 * Set the (temporary) highlight area
 */
dendrogramViewer.prototype.highlightArea = function(boundingBox) {
    this.currentHoverHighlight = boundingBox;
}

/**
 * Set the bounding box of the current permanent highlight area
 */
dendrogramViewer.prototype.setPermanentHighlightArea = function(area) {
    this.currentPermanentHightlight = area;
}

/**
 * Get the bounding box of the current permanent Hightlight area
 */
dendrogramViewer.prototype.getPermanentHighlightArea = function() {
    return this.currentPermanentHightlight;
}


dendrogramViewer.prototype.clearPermanentHighlightArea = function() {
    this.currentPermanentHightlight = undefined;
}

/**
 * Clear the overlay hightlight area
 */
dendrogramViewer.prototype.clearHighlight = function() {
    this.currentHoverHighlight = undefined;
}


/**
 * Initialise the dendrogram viewer:
 * @description set the canvas element width and height; Set up the
 * click listener; Call the first draw function. Assumes that <canvas id="dendrogram-area">
 *
 */
dendrogramViewer.prototype.initialize = function() {
    this.setupDOM();
    this.setupListeners();

    this.updateCanvasSize();

    // Initialise buttons and draw the dendrogram
    this.initializeButtons();
    this.drawDendrogram();
};

/**
 * Re-draw the dendrogram with the current configuration
 */
dendrogramViewer.prototype.redrawDendrogram = function() {
    var config = this.currentConfiguration.view;

    if (config === "zoom") {
        this.drawDendrogram(this.getZoomNode());
    } else if (config === "full") {
        this.drawDendrogram();
    } else {
        console.error("Unknown dendrogram configuration: ", config);
    }
}

/*
 * Show popup for initiating the download of the different images
 */
dendrogramViewer.prototype.downloadImage = function() {
    var canvas = document.getElementById('dendrogram-area');

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
                        pagHelpers.downloadURL(data, 'dendrogram.png', canvas)
                    })
                } //if
            } //fn
        }) // Ext.Msg.show
    } else {
        canvas.toBlob(function(data) {
            pagHelpers.downloadURL(data, 'dendrogram.png', canvas)
        })
    }
}

/**
 * Replace the current main heatmap genes with related genes
 */
dendrogramViewer.prototype.showRelatedGenes = function() {
    // Get the current displayed genes
    var gsc = new geneSelectionController();
    var sel = gsc.getSelection('auto_heatmapDisplayGenes');

    var showError = function() {
        Ext.Msg.show({
            title: 'Warning',
            msg: 'No genes are currently displayed on the main heatmap.',
            buttons: Ext.Msg.OK
        });
    };

    if (typeof(sel) != 'undefined') {
        var queryGenes = sel.genes;
        if (queryGenes.length > 0) {
            var dc = new dataController();
            try {
                dc.getGeneNeighbours(queryGenes, function(results) {

                    // TODO: check that we got at least one gene back

                    var geneSelCntr = new geneSelectionController();
                    geneSelCntr.setSelection(results, 'relatedSelection', 'relatedSelection');

                    var heatmapV = new heatmapViewer();
                    heatmapV.setNamedSelectionToDisplayGenes('auto_relatedSelection');
                    heatmapV.drawHeatmap();

                })

            } catch (e) {
                if (e.code == STATIC_FILE_FIELD_MISSING) {

                    Ext.Msg.show({
                        title: 'Error',
                        msg: 'The loaded file does not support finding similar gene patterns',
                        buttons: Ext.Msg.OK
                    });
                } else {
                    console.error("An unknown error occured");
                }
            }
        } else if (queryGenes.length > 101) {
            Ext.Msg.show({
                title: 'Warning',
                msg: 'The heatmap is currently displaying over 100 genes, please reduce and try again.',
                buttons: Ext.Msg.OK
            });
        } else {
            showError()
        }
    } else { // selection undefined
        showError()

    }

}

dendrogramViewer.prototype.downloadImagePopUp = function() {
    (new dendrogramViewer()).downloadImage();
    (new metaDataHeatmapViewer()).downloadImage();
    (new aspectHeatmapViewer()).downloadImage();
    (new heatmapViewer()).downloadImage();
}
