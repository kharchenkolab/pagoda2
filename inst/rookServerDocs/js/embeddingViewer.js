"use strict";

/**
 * Responsible for for handling the embedding panel
 * This class does not actually do any plotting. It delegates the plotting
 * to a suitable class depending on the type of the embedding. It does
 * however maintain the plot state, the individual embedders should not
 * maintain state
 * Singleton
 * @constructor
 */
function embeddingViewer() {
    if (typeof embeddingViewer.instance === 'object') {
        return embeddingViewer.instance;
    };

    // Singleton
    embeddingViewer.instance = this;

    this.generateToolbar();

    this.loadDefaultConfig();

    // Make default viewer
    this.currentViewer = new embeddingViewerScatterCanvas();

    // Generate the wait box for showing later
    this.generateWait();
}; // function embeddingViewer() {

/**
 * Shows the wait overlay
 */
embeddingViewer.prototype.showWait = function() {
    var extJsContainer = Ext.getCmp('embeddingExtJSWrapper');
    var plotHeight = extJsContainer.body.getHeight(true);
    var plotWidth = extJsContainer.body.getWidth(true);

    var embWaitDiv = $('#embedding-wait');

    var top = (plotHeight - embWaitDiv.height()) / 2;
    var left = (plotWidth - embWaitDiv.width()) / 2;

    embWaitDiv.css({
        'visibility': 'visible',
        'top': top,
        'left': left
    });
}

/**
 * Hides the wait overlay
 */
embeddingViewer.prototype.hideWait = function() {
    $('#embedding-wait').css('visibility', 'hidden');
}

/**
 * Generates the wait overlay div and leaves it hidden for future use
 * @private
 */
embeddingViewer.prototype.generateWait = function() {
    $('#embedding-draw-outer').append('<div id="embedding-wait" style="background-color: rgba(127,127,127,0.9); width: 200px; height: 90px; z-index:3; position: absolute; top: 0px; left: 0px; visibility: hidden; border-radius: 10px;"><p style="text-align:center; padding-top: 10px; color: white; font-weight: bold">Loading...</p><img style="position: absolute; top: 45px; left: 89px;" class="loadingIcon" src="img/loading.gif"/></div>');
}

/**
 * Loads the default configuration from p2globalParams
 * @private
 */
embeddingViewer.prototype.loadDefaultConfig = function() {
    this.currentConfiguration = {};
    this.setCurrentPointSize(p2globalParams.embedding.defaultPlotPointSize);
    this.currentConfiguration.currentAlpha = p2globalParams.embedding.defaultAlpha;
    this.currentConfiguration.border = p2globalParams.embedding.defaultBorder;
    this.currentConfiguration.borderColor = p2globalParams.embedding.defaultBorderColor;
    this.currentConfiguration.borderWidth = p2globalParams.embedding.defaultBorderWidth;
}

/**
 * Generate the toolbar for the embedding viewer and populate
 * it with options from the dataController
 */
embeddingViewer.prototype.generateToolbar = function() {

    // Define a store for the options
    var embeddingOptionsStore = Ext.create('Ext.data.Store', {
        fields: ['reduction', 'embedding', 'label', 'value'],
        id: 'embeddingOptionsStore'
    });

    // Make a combobox
    var comboBox = Ext.create('Ext.form.ComboBox', {
        store: embeddingOptionsStore,
        queryMode: 'local',
        displayField: 'label',
        editable: false,
        valueField: 'value',
        id: 'embeddingSelectCombo',
        forceSelection: true
    });

    // Set the option change listener
    comboBox.addListener('change', function(e) {
        var value = e.value;
        var embeddingIds = value.split(':');
        var embView = new embeddingViewer();
        embView.showEmbedding(embeddingIds[0], embeddingIds[1]);
    });

    // Menu for embedding settings
    var embeddingSettingsMenu = Ext.create('Ext.menu.Menu', {
        id: 'embeddingSettingsMenu',
        items: [{
                fieldLabel: 'Point size',
                xtype: 'numberfield',
                tooltip: 'Plot point size',
                value: p2globalParams.embedding.defaultPlotPointSize,
                minValue: 0.1,
                maxValue: 20,
                disabled: false,
                listeners: {
                    change: {
                        buffer: 800,
                        fn: function(f, v) {
                            var embViewer = new embeddingViewer();
                            embViewer.setCurrentPointSize(v);
                            embViewer.redraw();
                        }
                    } // change buffered listener
                } // listeners
            },
            {
                fieldLabel: 'Opacity',
                xtype: 'numberfield',
                tooltip: 'Plot point opacity',
                value: p2globalParams.embedding.defaultAlpha,
                minValue: 0.0001,
                maxValue: 1.0,
                step: 0.1,
                disabled: false,
                listeners: {
                    change: {
                        buffer: 800,
                        fn: function(f, v) {
                            var embViewer = new embeddingViewer();
                            embViewer.setCurrentAlpha(v);
                            embViewer.redraw();
                        }
                    } // change
                } // listeners
            },
            {
                text: 'Show border',
                checked: p2globalParams.embedding.defaultBorder,
                checkHandler: function(f, v) {
                    var embViewer = new embeddingViewer();
                    embViewer.setCurrentBorder(v);
                    embViewer.redraw();
                }
            },
            {
                fieldLabel: 'Border width',
                xtype: 'numberfield',
                tooltip: 'Plot point border width',
                value: p2globalParams.embedding.defaultBorderWidth,
                minValue: 0.001,
                maxValue: 3,
                step: 0.25,
                disabled: false,
                listeners: {
                    change: {
                        buffer: 800,
                        fn: function(f, v) {
                            var embViewer = new embeddingViewer();
                            embViewer.setCurrentBorderWidth(v);
                            embViewer.redraw();
                        }
                    } // change and buffer
                }
            }
        ], // items of embeddingSettingsMenu
    });

    // Add it to a toolbar in the appropriatepanel
    var embeddingPanel = Ext.getCmp('embeddingExtJSWrapper');
    var embeddingPanelHeader = embeddingPanel.getHeader();
    var toolbar = Ext.create('Ext.Toolbar');
    toolbar.add(comboBox);
    toolbar.add({
        xtype: 'tbseparator'
    });
    toolbar.add({
        text: "",
        type: "button",
        tooltip: 'Select With Box',
        glyph: 0xf0c8,
        id: "boxSelectionButton",
        disabled: true,
        handler: function() {
            Ext.getCmp("boxSelectionButton").disable();
            Ext.getCmp("polygonSelectionButton").enable();
            (new embeddingViewer()).currentViewer.highlight = "box";
        }
    })
    toolbar.add({
        text: "",
        type: "button",
        tooltip: 'Select With Polygon',
        id: "polygonSelectionButton",
        glyph: 0xf040,
        handler: function() {
            Ext.getCmp("boxSelectionButton").enable();
            Ext.getCmp("polygonSelectionButton").disable();
            (new embeddingViewer()).currentViewer.highlight = "poly";
        }
    });
    toolbar.add({
        xtype: 'tbseparator'
    });
    // Add a button for the settings menu
    toolbar.add({
        text: "",
        type: "button",
        tooltip: 'Download current view',
        glyph: 0xf0ed,
        handler: function() {

            //set up background canvas for rendering of the print preview and a high definition saved image
            var aspV = new aspectHeatmapViewer();
            var embV = new embeddingViewer();
            var backgroundCanvas = document.createElement("canvas");
            backgroundCanvas.height = 1000;
            backgroundCanvas.width = 1000;
            var backgroundOverlay = document.createElement("canvas");
            backgroundOverlay.height = 1000;
            backgroundOverlay.width = 1000;
            var embViewSC = (new embeddingViewer()).currentViewer;
            embViewSC.drawToCanvas(backgroundCanvas, 1000);
            if (aspV.currentOverlaySelectionShown) {
                if (aspV.currentOverlaySelectionName !== null) {
                    embViewSC.highlightSelectionsByNamesOntoCanvas(backgroundOverlay, 1000, Array(aspV.currentOverlaySelectionName));
                } else {
                    embViewSC.highlightSelectionsByNamesOntoCanvas(backgroundOverlay, 1000, aspV.currentOverlaySelectionNames);
                }
            }

            var overlay = document.getElementById('embedding-canvas-overlay');

            //Window containing print preview and aditional image altering options
            Ext.create("Ext.window.Window", {
                title: "Embedding Download Preview",
                id: "EmbeddingEditor",
                layout: 'hbox',
                constrain: true,
                closable: true,
                resizable: false,
                height: 300,
                width: 540,
                items: [{
                        xtype: 'container',
                        title: 'Display Options',
                        margin: '6 6 6 6',
                        items: [{
                                xtype: 'checkbox',
                                boxLabel: 'Include Title',
                                id: 'includeTitle',
                                listeners: {
                                    //adds paddding for title and does some reformatting
                                    change: function() {
                                        var me = Ext.getCmp('includeTitle');
                                        if (Ext.getCmp('includeTitle').getValue()) {
                                            Ext.getCmp('embTitle').enable();
                                            Ext.getCmp('title-font-size').enable();
                                        } else {
                                            Ext.getCmp('embTitle').disable();
                                            Ext.getCmp('title-font-size').disable();
                                        }

                                        Ext.getCmp('EmbeddingEditor').refresh();
                                    }
                                }
                            },
                            {
                                xtype: 'textfield',
                                id: 'embTitle',
                                fieldLabel: 'Title',
                                disabled: true,
                                listeners: {
                                    change: function() {
                                        Ext.getCmp('EmbeddingEditor').refresh();
                                    }
                                }
                            }, //title
                            {
                                xtype: 'numberfield',
                                id: 'title-font-size',
                                label: 'Title Font Size',
                                value: 100,
                                maxValue: 115,
                                minValue: 10,
                                disabled: true
                            },
                            {
                                xtype: 'checkbox',
                                boxLabel: 'Include Axis',
                                id: 'includeAxis',
                                listeners: {
                                    change: function() {
                                        if (Ext.getCmp('includeAxis').getValue()) {
                                            Ext.getCmp('xAxisTitle').enable();
                                            Ext.getCmp('yAxisTitle').enable();
                                            Ext.getCmp('axis-font-size').enable();
                                        } else {
                                            Ext.getCmp('xAxisTitle').disable();
                                            Ext.getCmp('yAxisTitle').disable();
                                            Ext.getCmp('axis-font-size').disable();
                                        }

                                        Ext.getCmp('EmbeddingEditor').refresh();
                                    }
                                }
                            },
                            {
                                xtype: 'textfield',
                                id: 'xAxisTitle',
                                fieldLabel: 'X-Axis Title',
                                disabled: true,
                                listeners: {
                                    change: function() {
                                        Ext.getCmp('EmbeddingEditor').refresh();
                                    }
                                }
                            },
                            {
                                xtype: 'textfield',
                                id: 'yAxisTitle',
                                fieldLabel: 'Y-Axis Title',
                                disabled: true,
                                listeners: {
                                    change: function() {
                                        Ext.getCmp('EmbeddingEditor').refresh();
                                    }
                                }
                            },
                            {
                                xtype: 'numberfield',
                                id: 'axis-font-size',
                                label: 'Axis Font Size',
                                value: 60,
                                maxValue: 80,
                                minValue: 10,
                                disabled: true
                            },
                            {
                                xtype: 'checkbox',
                                boxLabel: 'Include Highlights',
                                id: 'includeHighlight',
                                listeners: {
                                    change: function() {
                                        Ext.getCmp('EmbeddingEditor').refresh();
                                    }
                                }

                            },
                            {
                                xtype: 'button',
                                text: 'Save Image',
                                id: 'saveImage-embedding-download',
                                margin: '4 4 4 4',
                                handler: function() {
                                    var printCanvas = document.createElement("canvas")
                                    printCanvas.height = "1000";
                                    printCanvas.width = "1000";
                                    printCanvas.id = "print-embedding";
                                    Ext.getCmp("EmbeddingEditor").drawPlot(backgroundCanvas, overlay, printCanvas, 1000);
                                    const maxSize = 2000;
                                    if (printCanvas.width > maxSize | printCanvas.height > maxSize) {
                                        Ext.Msg.show({
                                            title: 'Warning',
                                            msg: 'The current canvas size exceeds ' + maxSize + 'px in at least one dimention.' +
                                                'This may cause problems during exporting. Do you want to continue?',
                                            buttons: Ext.Msg.OKCANCEL,
                                            fn: function(s) {
                                                if (s == 'ok') {
                                                    printCanvas.toBlob(function(data) {
                                                        pagHelpers.downloadURL(data, 'embedding.png', printCanvas)
                                                    })
                                                } //if
                                            } //fn
                                        }) // Ext.Msg.show
                                    } else {
                                        printCanvas.toBlob(function(data) {
                                            pagHelpers.downloadURL(data, 'embedding.png', printCanvas)
                                        })
                                    } // if
                                    Ext.getCmp("EmbeddingEditor").close()
                                }
                            },
                            {
                                xtype: 'button',
                                text: 'Cancel',
                                id: 'cancel-embedding-download',
                                margin: '4 4 4 4',
                                handler: function() {
                                    Ext.getCmp("EmbeddingEditor").close();
                                }
                            },
                            {
                                xtype: 'button',
                                text: 'Refresh',
                                id: 'refresh-canvases',
                                margin: '4 4 4 4',
                                handler: function() {
                                    embViewSC.drawToCanvas(backgroundCanvas, 1000);
                                    if (aspV.currentOverlaySelectionShown) {
                                        if (aspV.currentOverlaySelectionName !== null) {
                                            embViewSC.highlightSelectionsByNamesOntoCanvas(backgroundOverlay, 1000, Array(aspV.currentOverlaySelectionName));
                                        } else {
                                            embViewSC.highlightSelectionsByNamesOntoCanvas(backgroundOverlay, 1000, aspV.currentOverlaySelectionNames);
                                        }
                                    }
                                    Ext.getCmp("EmbeddingEditor").refresh();
                                }
                            }
                        ]
                    },

                    {
                        xtype: 'container',
                        title: 'Preview',
                        margin: '6 6 6 6',
                        items: [{
                            html: '<canvas id="print-preview-canvas" height="250" width="250"></canvas>'
                        }]
                    }
                ],
                refresh: function() {
                    this.drawPlot(backgroundCanvas, backgroundOverlay, document.getElementById("print-preview-canvas"), 250);
                },

                //draws the plot from a canvas onto a destination canvas with the fixtures specified by the user included
                drawPlot: function(canvas, overlay, destination, squareDim) {
                    var options = [Ext.getCmp('includeTitle').getValue(), Ext.getCmp('includeAxis').getValue(), Ext.getCmp('includeHighlight').getValue()];


                    var targetContext = destination.getContext("2d");
                    targetContext.clearRect(0, 0, squareDim, squareDim);
                    targetContext.fillStyle = "#FFFFFF";
                    targetContext.fillRect(0, 0, squareDim, squareDim);
                    if (!(options[0] || options[1])) {
                        destination.getContext("2d").drawImage(backgroundCanvas, 2, 2, squareDim - 4, squareDim - 4);
                        if (options[2]) {
                            destination.getContext("2d").drawImage(backgroundOverlay, 2, 2, squareDim - 4, squareDim - 4);
                        }
                        return;
                    }
                    var readablePadding = 2;
                    var titlePadding = Math.ceil(squareDim / 125);
                    var graphPaddingLeft = 2;
                    var graphPaddingRight = 10;
                    var graphPaddingTop = 10;
                    var graphPaddingBottom = 2;
                    var lineThickness = 1;

                    var axisFontSize = Ext.getCmp('axis-font-size').getValue() / (1000 / squareDim);
                    var titleFontSize = Ext.getCmp('title-font-size').getValue() / (1000 / squareDim);
                    var text = [Ext.getCmp('embTitle').getValue(), Ext.getCmp('xAxisTitle').getValue(), Ext.getCmp('yAxisTitle').getValue()]
                    var topOffset = (options[0] ? axisFontSize + titlePadding * 2 : 0);
                    var bottomOffset = (options[1] ? titleFontSize : 0);
                    var leftOffset = (options[1] ? axisFontSize : 0);
                    var enclosingMargin = 10;
                    var plotDim = Math.min(squareDim - (topOffset + bottomOffset + 2 * enclosingMargin), squareDim - (leftOffset + 2 * enclosingMargin));



                    var topLeft = {
                        x: leftOffset + enclosingMargin,
                        y: topOffset + enclosingMargin
                    }
                    //clear the canvas


                    //draw the plot
                    targetContext.drawImage(canvas, topLeft.x + graphPaddingLeft, topLeft.y + graphPaddingTop, plotDim - (graphPaddingLeft + graphPaddingRight), plotDim - (graphPaddingTop + graphPaddingBottom));

                    targetContext.font = titleFontSize + "px Arial"
                    targetContext.fillStyle = "#000000";
                    targetContext.textAlign = "center";
                    if (options[0]) {
                        targetContext.fillText(text[0], (topLeft.x + plotDim / 2), topLeft.y - titlePadding);
                    }
                    //draw necessary axis
                    targetContext.font = axisFontSize + "px Arial";
                    targetContext.textAlign = "center"

                    //draw X axis
                    if (options[1]) {

                        targetContext.beginPath();
                        targetContext.textBaseline = "top";
                        //targetContext.fillRect(topLeft.x-lineThickness, topLeft.y + plotDim, plotDim + lineThickness, lineThickness);
                        targetContext.fillText(text[1], topLeft.x + plotDim / 2, topLeft.y + plotDim + lineThickness + readablePadding);
                        pagHelpers.canvas_arrow(targetContext, topLeft.x, topLeft.y + plotDim, topLeft.x + plotDim, topLeft.y + plotDim, 10 * (squareDim / 250));

                        //draw Y axis
                        //targetContext.fillRect(topLeft.x-lineThickness, topLeft.y, lineThickness,  plotDim + lineThickness);

                        pagHelpers.canvas_arrow(targetContext, topLeft.x, topLeft.y + plotDim - lineThickness, topLeft.x, topLeft.y, 10 * (squareDim / 250));
                        targetContext.textBaseline = "bottom";
                        targetContext.rotate(-Math.PI / 2);
                        targetContext.fillText(text[2], -(topLeft.y + plotDim / 2), topLeft.x - readablePadding);
                        targetContext.rotate(Math.PI / 2);

                        targetContext.stroke();
                        targetContext.closePath();
                    }

                    if (options[2]) {
                        targetContext.drawImage(overlay, topLeft.x + graphPaddingLeft, topLeft.y + graphPaddingTop, plotDim - (graphPaddingLeft + graphPaddingRight), plotDim - (graphPaddingTop + graphPaddingBottom));
                    }
                }
            }).show()

            document.getElementById('print-preview-canvas').getContext("2d").drawImage(backgroundCanvas, 2, 2, 246, 246)

        } // handler
    }); // toolbar add

    toolbar.add({
        text: "",
        tooltip: "Information about View",
        glyph: 0xf05a,
        handler: function() {

            var ev = new embeddingViewer();
            var colorConfig = ev.getColorConfiguration();

            var colorConfigHtml;
            if (colorConfig == 'metadata') {
                colorConfigHtml = " coloured by metadata of type " + ev.getMetadataColorInfo().metadataName + ".";
            } else if (colorConfig == 'aspect') {
                colorConfigHtml = " coloured by aspect of type '" + ev.getAspectColorInfo().aspectid + "'.";
            } else if (colorConfig == 'geneexpression') {
                colorConfigHtml = " coloured by expression of gene '" + ev.getGeneExpressionColorInfo().geneid + "'.";
            } else {
                colorConfigHtml = '.'
            }


            var config = ev.getConfig();
            var html = "<p>You are currently viewing embedding " + config.type + "-->" + config.embeddingType +
                colorConfigHtml + "</p>";

            Ext.create('Ext.window.Window', {
                height: 150,
                width: 400,
                title: 'Embedding Info',
                scrollable: true,
                bodyPadding: 10,
                html: html,
                constrain: true,
                closable: true,
                resizable: false
            }).show();

        }
    }); // toolbar add

    toolbar.add({
        text: '',
        type: 'button',
        tooltip: 'Clear selection',
        glyph: 0xf12d,
        handler: function() {
            var embV = new embeddingViewer();
            embV.clearHighlight();
        }
    });

    toolbar.add({
        text: "",
        type: "button",
        tooltip: "Customize embedding view",
        glyph: 0xf013,
        menu: embeddingSettingsMenu
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
                title: 'Help: Embedding',
                scrollable: true,
                bodyPadding: 10,
                html: '<h2>Embedding </h2>' +
                    '<p>The embedding displays the cells as points in a 2d or 3d layout.</p>' +
                    '<p>Pagoda2 allows you to switch between multiple embeddings using the drop-down box at the top.' +
                    'You can select cells on the current embedding by dragging with your mouse.' +
                    'This will highlight the selected cells in the embedding and other windows. ' +
                    'You can download the embedding you are currently viewing by ' +
                    'clicking on the <span style="font-family: FontAwesome">&#xf0ed</span> (download) icon. In some cases the downloaded file will not have the correct extension, please rename it to end in ".png" if that happens. You can check the what colors you have currently applied to the embedding by clicking on the <span style="font-family: FontAwesome">&#xf0f6</span>  (Current Display Information) icon.' +
                    'You can clear the selection by clicking on the <span style="font-family: FontAwesome">&#xf12d</span> (clear) icon.' +
                    'You can use the <span style="font-family: FontAwesome">&#xf013</span> (settings) icon to change the viewing settings of the current embedding. ' +
                    'This allows you to customize point size, opacity and border color and width.' +
                    'In addition the embedding can be colored using the heatmaps on the right. See the ' +
                    'help windows of those panels for more information.</p>',
                constrain: true,
                closable: true,
                resizable: false
            }).show();
        } // handler
    }); // toolbar add


    embeddingPanelHeader.add(toolbar);

    this.populateMenu();

}; // generateToolbar


/**
 * Clear the highlight
 */
embeddingViewer.prototype.clearHighlight = function() {
    this.currentViewer.clearHighlight();
}

/**
 * Populates the available embeddings menu
 * @private
 * @todo this is slow, and generates many requests we should remodel
 * the data controller to get all the data that we want with
 * one request, also account for the fact that some embeddings
 * might not correspond to reduction
 * Also embedding will now need to have a plot type
 */
embeddingViewer.prototype.populateMenu = function() {
    var datCntr = new dataController();
    datCntr.getEmbeddingStructure(function(d) {
        var embeddingOptionstore = Ext.data.StoreManager.lookup('embeddingOptionsStore');
        for (var reduction in d) {
            for (var embedding in d[reduction]) {
                var embeddingIdentifier = reduction + ':' + embedding;
                var embeddingLabel = reduction + ' \u21e8 ' + embedding;

                embeddingOptionstore.add({
                    'reduction': reduction,
                    'embedding': embedding,
                    "label": embeddingLabel,
                    "value": embeddingIdentifier
                });

                if (reduction === p2globalParams.embedding.defaultEmbedding.reduction &&
                    embedding === p2globalParams.embedding.defaultEmbedding.embedding) {
                    Ext.getCmp('embeddingSelectCombo').setValue(embeddingIdentifier);
                }
            }
        }
    });
}

/**
 * Updates current plot colors to reflect dendrogram selection
 * @private
 */
embeddingViewer.prototype.updateColorsDendrogram = function() {
    this.currentViewer.updateColorsDendrogram();
}

/**
 * Update the embedding colors to match metadata information
 */
embeddingViewer.prototype.updateColorsMetadata = function() {
    this.currentViewer.updateColorsMetadata();
}

/**
 * Update the embedding colors to dispaly the expression of a gene
 */
embeddingViewer.prototype.updateColorsGeneexpression = function() {
    this.currentViewer.updateColorsGeneexpression();
}

/**
 * Update the current colors, some embedders optimise this
 * operation and do nor redraw (e.g. SVG)
 */
embeddingViewer.prototype.updateColors = function() {
    this.currentViewer.updateColors();
}

/**
 * Update to show a specific embedding
 * @param type the reduction from which to get this embedding from
 * @param embeddingType the embedding type for this reduction to show
 */
embeddingViewer.prototype.showEmbedding = function(type, embeddingType) {
    this.currentConfiguration.type = type;
    this.currentConfiguration.embeddingType = embeddingType;
    this.plotEmbedding();
}

/**
 * Plot the embedding from scratch
 */
embeddingViewer.prototype.plotEmbedding = function() {
    this.currentViewer.plotEmbedding();
}

/**
 * Highlight the selection specified by name, delegates to current viewer
 */
embeddingViewer.prototype.highlightSelectionByName = function(selectionName, hasLabels) {
    this.currentViewer.highlightSelectionByName(selectionName, hasLabels);
}

embeddingViewer.prototype.highlightSelectionsByNames = function(selectionNames, hasLabels) {
    this.currentViewer.highlightSelectionsByNames(selectionNames, hasLabels);
}

/////////////////////////////////////
// Getter and Setters follow
/////////////////////////////////////
/**
 * Get the current configuration
 */
embeddingViewer.prototype.getConfig = function() {
    return this.currentConfiguration;
}

/**
 * Set the overall color configuration, what is the embedding colored by. Different configurations
 * have their own conventions for passing information about the coloring of individual nodes
 * for example the dendrogram uses cell selections and the metadata pass the metadata type
 * seperately by the setMetaDataColorInfo() function
 * @param colorConfig {string} the type of color configuration,
 * one of 'dendrogram', 'metadata', 'geneexpression'
 */
embeddingViewer.prototype.setColorConfiguration = function(colorConfig) {
    this.currentConfiguration.colors = colorConfig;
}

/**
 * Get the color configuration
 */
embeddingViewer.prototype.getColorConfiguration = function() {
    return this.currentConfiguration.colors;
}

/**
 * Get the extra information required for plotting gene colors
 */
embeddingViewer.prototype.getGeneExpressionColorInfo = function() {
    return this.currentConfiguration.geneexpressionColorInfo;
}

/**
 * Set the extra information required for plotting gene colors
 */
embeddingViewer.prototype.setGeneExpressionColorInfo = function(colorInfo) {
    this.currentConfiguration.geneexpressionColorInfo = colorInfo;
}

/**
 * Set the extra information required for plottting aspect colors
 */
embeddingViewer.prototype.setAspectColorInfo = function(colorInfo) {
    this.currentConfiguration.aspectColorInfo = colorInfo;
}

/**
 * Get the extra information required for plotting aspect colors
 */
embeddingViewer.prototype.getAspectColorInfo = function() {
    return this.currentConfiguration.aspectColorInfo;
}

/**
 * Set the extra information required for plotting dendrogram colors
 */
embeddingViewer.prototype.setDendrogramColorInfo = function(colorInfo) {
    this.currentConfiguration.dendrogramColorInfo = colorInfo;
}

/**
 * Get the extra information required for plotting dendrogram colors
 */
embeddingViewer.prototype.getDendrogramColorInfo = function() {
    return this.currentConfiguration.dendrogramColorInfo;
}

/**
 * Set extra information required for coloring by metadata
 */
embeddingViewer.prototype.setMetadataColorInfo = function(colorInfo) {
    this.currentConfiguration.metaDataColorInfo = colorInfo;
}

/**
 * Get the extra information required for coloring by metadata
 */
embeddingViewer.prototype.getMetadataColorInfo = function() {
    return this.currentConfiguration.metaDataColorInfo;
}

/**
 * Set the alpha value used to plot
 */
embeddingViewer.prototype.setCurrentAlpha = function(alpha) {
    this.currentConfiguration.currentAlpha = alpha;
}

/**
 * Set the current border
 */
embeddingViewer.prototype.setCurrentBorder = function(border) {
    this.currentConfiguration.border = border;
}

/**
 * Get the current border
 */
embeddingViewer.prototype.getCurrentBorder = function() {

    return this.currentConfiguration.border;
}

/**
 * Get the current border color
 */
embeddingViewer.prototype.getCurrentBorderColor = function() {
    return this.currentConfiguration.borderColor;
}

/**
 * Set the current  border color
 */
embeddingViewer.prototype.setCurrentBorderColor = function(borderColor) {
    this.currentConfiguration.borderColor = borderColor
}

/**
 * Get  current border width
 */
embeddingViewer.prototype.getCurrentBorderWidth = function() {
    return this.currentConfiguration.borderWidth;
}

/**
 * Set current border width
 */
embeddingViewer.prototype.setCurrentBorderWidth = function(borderWidth) {
    this.currentConfiguration.borderWidth = borderWidth;
}

/**
 * Get the alpha value used to plot
 */
embeddingViewer.prototype.getCurrentAlpha = function() {
    return this.currentConfiguration.currentAlpha;
}

/**
 * Get the point size to plot with
 */
embeddingViewer.prototype.getCurrentPointSize = function() {
    return this.currentConfiguration.pointsize;
}

/**
 * Set the point size to plot with
 */
embeddingViewer.prototype.setCurrentPointSize = function(sz) {
    this.currentConfiguration.pointsize = sz;
}
