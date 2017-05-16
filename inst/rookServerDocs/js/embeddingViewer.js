

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

    console.log('Initialising embeddingViewer...');

    // Singleton
    embeddingViewer.instance = this;

    // FIXME: This needs to be done before the event listeners are configured
    // and after the instance is set to this. This is a race condition
    // for the backend
    this.generateToolbar();

    this.loadDefaultConfig();

    // Generate and show a viewer, save it in the current viewer variable
    // so we can send commands to it later

    // NOTE: This is the original d3 viewer with performace issues
    //this.currentViewer = new scatterEmbeddingD3();


    // If we ever implement multiple viewers the appropriate containers
    // need to be cleanedup
    // This is the currentViewer that plots with canvas
    this.currentViewer = new embeddingViewerScatterCanvas();


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

    var top = (plotHeight - embWaitDiv.height() ) /2;
    var left = (plotWidth - embWaitDiv.width() ) /2;

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
 */
embeddingViewer.prototype.generateWait = function() {
    $('#embedding-draw-outer').append('<div id="embedding-wait" style="background-color: rgba(127,127,127,0.9); width: 200px; height: 90px; z-index:3; position: absolute; top: 0px; left: 0px; visibility: hidden; border-radius: 10px;">'+
'<p style="text-align:center; padding-top: 10px; color: white; font-weight: bold">Loading...</p>'
+'<img style="position: absolute; top: 45px; left: 89px;" class="loadingIcon" src="img/loading.gif"/></div>');
}


/**
 * Loads the default configuration
 * @private
 */
embeddingViewer.prototype.loadDefaultConfig = function () {
    this.currentConfiguration = { };
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
	fieldLabel: 'Select Embedding',
	store: embeddingOptionsStore,
	queryMode: 'local',
	displayField: 'label',
	editable: false,
	valueField: 'value'
    });

    // Set the option change listener
    comboBox.addListener('change', function(e) {
	var value =  e.value;
	var embeddingIds = value.split(':');
	embView = new embeddingViewer();
	embView.showEmbedding(embeddingIds[0],embeddingIds[1]);
    });

    // Menu for embedding settings
    var embeddingSettingsMenu = Ext.create('Ext.menu.Menu', {
	id: 'embeddingSettingsMenu',
	items: [{
	    fieldLabel: 'Point size',
	    xtype: 'numberfield',
	    tooltip: 'Plot point size',
	    value: p2globalParams.embedding.defaultPlotPointSize, // 2
	    minValue: 0.1,
	    maxValue: 20,
	    disabled: false,
	    listeners: {
		change: {buffer: 800, fn: function(f,v){
		    var embViewer = new embeddingViewer();
		    embViewer.setCurrentPointSize(v);
		    embViewer.redraw();
		}} // change buffered listerne
	    }// listeners
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
		change: {buffer: 800, fn: function(f,v){
		    var embViewer =  new embeddingViewer();
		    embViewer.setCurrentAlpha(v);
		    embViewer.redraw();
		}} // change
	    } // listeners
	},
	{
	    text: 'Show border',
	    checked: p2globalParams.embedding.defaultBorder,
	    checkHandler: function(f, v){
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
		change: {buffer: 800, fn: function(f, v){
		    var embViewer = new embeddingViewer();
		    embViewer.setCurrentBorderWidth(v);
		    embViewer.redraw();
		}} // change
	    }
	}
	], // items
    });

    // Add it to a toolbar in the appropriatepanel
    var embeddingPanel = Ext.getCmp('embeddingExtJSWrapper');
    var embeddingPanelHeader = embeddingPanel.getHeader();
    var toolbar = Ext.create('Ext.Toolbar');
    toolbar.add(comboBox);
    toolbar.add({xtype: 'tbseparator'});
    // Add a button for the settings menu
    toolbar.add(
      {
        text: "",
        type: "button",
        tooltip: 'Download current view',
        glyph: 0xf0ed,
        handler: function(){
              var embV = new embeddingViewer();
              var canvas = embV.currentViewer.getMainCanvasElement();


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
                              window.open(imageURL,'embeddingWindow');
                         } //if
                       } //fn
                    }) // Ext.Msg.show
                } else {
                    var imageURL = canvas.toDataURL('image/png');
                    imageURL = imageURL.replace(/^data:image\/[^;]*/, 'data:application/octet-stream');
                    window.open(imageURL);
                }// if
        } // handler
      }); // toolbar add

toolbar.add(
  {
    text: '',
    type: 'button',
    tooltip: 'Clear selection',
    glyph: 0xf12d,
    handler: function() {
      var embV = new embeddingViewer();
      embV.clearHighlight();
    }
  }
  );

    toolbar.add(
    	{
    	    text: "",
    	    type: "button",
    	    tooltip: "Customize embedding view",
    	    glyph: 0xf013,
    	    menu: embeddingSettingsMenu
    	}
    );


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
                '<p>The embedding displays the cells as points in a 2d or 3d layout.</p>'  +
                '<p>Pagoda2 allows you to switch between multiple embeddings using the drop-down box at the top.' +
                'You can select cells on the current embedding by dragging with your mouse.' +
                'This will highlight the selected cells in the embedding and other windows. ' +
                'You can download the embedding you are currently viewing by ' +
                'clicking on the <span style="font-family: FontAwesome">&#xf0ed</span> (download) icon. In some cases the downloaded file will not have the correct extension, please rename it to end in ".png" if that happens.' +
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


    // Delay populating the menu above as this will not be used
    // as soon as the app loads and it helps reduce the inital swarm of requests
    setTimeout(this.populateMenu, 2000)

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
    // Populate the menu using the data controller -- async
    var datCntr = new dataController();
    datCntr.getAvailableReductionTypes(function(reductions){
	for ( var i = 0; i < reductions.length; i++ ) {
	    var currReduction = reductions[i];


	    datCntr.getAvailableEmbeddings(currReduction, function(embeddings, currReduction) {

		embeddingOptionstore =  Ext.data.StoreManager.lookup('embeddingOptionsStore');

		if (embeddings !== null) {

 		    for (var j = 0; j < embeddings.length; j++) {
			var embeddingIdentifier =  currReduction + ':' + embeddings[j];
			var embeddingLabel =  currReduction + ' --> ' + embeddings[j];

			if (currReduction === p2globalParams.embedding.defaultEmbedding.reduction &&
			    embeddings[j] === p2globalParams.embedding.defaultEmbedding.embedding) {
			    // The default
			}

			embeddingOptionstore.add({
			    'reduction': currReduction,
			    'embedding':  embeddings[j],
			    "label": embeddingLabel,
			    "value": embeddingIdentifier
			});

		    } // for

		}
	    }, currReduction // For the callback extra information
					  ) // getAvailableEmbeddings
	} //for
    }); // getAvailableReductionTypes
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
embeddingViewer.prototype.updateColors =  function() {
    this.currentViewer.updateColors();
} // function

/**
 * Update to show a specific embedding
 * @param type the reduction from which to get this embedding from
 * @param embeddingType the embedding type for this reduction to show
 */
embeddingViewer.prototype.showEmbedding = function(type, embeddingType) {
    this.currentConfiguration.type = type;
    this.currentConfiguration.embeddingType = embeddingType;

    this.plotEmbedding();
} // embeddingViewer.prototype.showEmbedding


/**
 * Redraw the current view
 * @description It just calles plotEmbedding
 * @deprecated Use plotEmbedding() instead; this was used in pre ExtJS code for throttling
 */
embeddingViewer.prototype.redraw = function() {
    this.plotEmbedding();
};


/**
 * Plot the embedding from scratch
 */
embeddingViewer.prototype.plotEmbedding = function() {
    this.currentViewer.plotEmbedding();
}

// Getter and Setters follow

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
    this.currentConfiguration.currentAlpha =  alpha;
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
    this.currentConfiguration.borderWidth =  borderWidth;
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
embeddingViewer.prototype.setCurrentPointSize = function (sz) {
    this.currentConfiguration.pointsize = sz;
}

/**
 * Highlight the selection specified by name, delegates to current viewer
 */
embeddingViewer.prototype.highlightSelectionByName = function(selectionName) {
  this.currentViewer.highlightSelectionByName(selectionName);
}
