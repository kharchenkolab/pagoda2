"use strict";

/*
 * Filename: pagoda2frontend.js
 * Author: Nikolas Barkas
 * Date: January 2017
 * Description: pagoda2 front end application javascript for
 *    analysis of large single cell datasets.
 */


Ext.require(['*']);
Ext.onReady(function() {
    // The following line doesn't work in initlise
    // where it should actually be
    Ext.QuickTips.init();

    // See here for icon codes
    // http://fontawesome.io/cheatsheet/
    Ext.setGlyphFontFamily('FontAwesome');

    initialise();

    initialise2();
});

/**
 * Second step of initialisation.
 * @description This function contains extjs operations that need to
 * be performed after the basic element hierarchy has been established.
 */
function initialise2() {


var tableViewToolbar = Ext.create('Ext.Toolbar');


    tableViewToolbar.add({
      text: "",
      type: 'button',
      tooltip: 'Help',
      glyph: 0xf128,
        handler: function() {
          Ext.create('Ext.window.Window', {
            height: 300,
            width: 400,
            title: 'Help: Table View',
            scrollable: true,
            bodyPadding: 10,
            html: '<h2>Help: Table View</h2>' +
              '<p>The table view allows you to view and search tabular data.</p><p>The All Genes view displays all gene in the loaded dataset. Genesets of interest displays pre-loaded genesets that usually include GO Terms and may include pre-calculated differential expression results. Aspects provides information about each set of genesets in each aspect and allows you to browse the indivual genes. Differential expression displays the results of differential expression that you can calculated during this session.</p><p>All search boxes support the use of case-insensitive regular expressions. You can learn more about regular expressions <a href="http://www.regular-expressions.info/" target="_blank">here</a> and <a href="https://en.wikipedia.org/wiki/Regular_expression">here</a>. For example if you want to search for multiple genes at once you can enter: "^geneA$|^geneB$|^geneC$" (without quotes).</p><p>Most tables support selection of multiple elements. In order to select a range click on the first item and then while holding the Shift key click on the last. In order to select only specific items you can use the Control key (or Cmd in Mac).</p>',
            constrain: true,
            closable: true,
            resizable: false
          }).show();
    } // handler
    });


  Ext.getCmp('tableExtJSWrapper').getHeader().add(tableViewToolbar);

}


/**
 * Reads the p2globalParams options for the app configuration required
 */
function getDataLoadingParams() {
    // connectionType optiosn: 'remoteServer', 'remoteFile', or 'localFile',

  if (p2globalParams.dataLoadingParams.configuration == "server") {
    var params = {
      connectionType: 'remoteServer'
    };
    return params;
  } else if (p2globalParams.dataLoadingParams.configuration == "fileremote-static") {
    var params= {
      connectionType: 'remoteFile',
      remoteFileUrl: p2globalParams.dataLoadingParams.fileRemoteURL
    }
    return params;
  } else if (p2globalParams.dataLoadingParams.configuration == "fileremote-from-url") {
    var url_string = window.location.href;
    var url = new URL(url_string);
    var fileURL = url.searchParams.get("fileURL")

    var params = {
      connectionType: 'remoteFile',
      remoteFileUrl: fileURL
    }
    return params;
  } else {
    throw new Error('Unknown data configuration');
  }
  return null;

}

///////////////////////////////////////////////////////

/**
 * Check browser version and initialise app if OK
 */
function initialise() {
    if (!pagHelpers.checkBrowser()) {
        pagHelpers.showNotSupportedBrowserWarning();
    } else {
      // Generate the overall layout
      generateExtJsLayout();

      // Show dialog to user that allows selecting data source
      // alternatively this could be in p2Params in some configs
      var loadParams = getDataLoadingParams();
      var dataCntr = new dataController(loadParams);


      // Initialize internal components


      // Calculation controllers are init from a factory that is singleton
      var calcCntr = new calculationController(true, true);// Both local and remote

      var evtBus = new eventBus();
      var stsBar = new statusBar();
      var selCntr = new cellSelectionController();
      //var infoBxCntr = new infoboxController();
      var geneSelCntr = new geneSelectionController();

      // Controller for cell selection UI
      var cellSelUICntr = new cellSelectionUIcontroller();
      var geneSelUICntr = new geneSelectionUIcontroller();
      var actionPanelUICntr = new actionPanelUIcontroller();

      // Set the page title
      document.title = p2globalParams.generalParams.applicationName;

      // Initialize page components
      var embView = new embeddingViewer();
      // Load the default embedding
      embView.showEmbedding(p2globalParams.embedding.defaultEmbedding.reduction,
  			  p2globalParams.embedding.defaultEmbedding.embedding);


      // Generate the tables
      var geneTable = new geneTableViewer();

      var geneSetsTable = new geneSetsTableViewer();
      var heatDendView = new heatmapDendrogramViewer();
      var aspTableView = new aspectsTableViewer();

      var diffExprTableView = new diffExprTableViewer();

      // Not used
      //  var odGeneTable = new odGeneTableViewer();

      // Update status bar
      stsBar.showMessage("Ready");
    }
};


/**
 * Generate the basic page layout with extJS framework
 * @description Place divs which will be used by the system to place components
 */
function generateExtJsLayout() {

    // Generation of some of these items could be
    // responsibility of individual objects, this function is really
    // meant only for the overall layout

    var statusBar = Ext.create('Ext.ux.StatusBar', {
	id: 'pagoda-status-bar',
	defaultText: 'Ready',
	height: '20px',
    });

    // These are the innermost panels that contain the divs
    // that the app will use. This layer exists to facilitate
    // transitioning to a window system
    var embedding =  Ext.create('Ext.panel.Panel', {
	layout: 'fit',
	id: 'embeddingExtJSWrapper',
	title: 'Embedding',
	header: true, //Show the innermost title
	padding: 0,
	border: 0,
	height: '100%',
	bodyPadding: 0,
	glyph: 0xf1cb,
	items: [{
	    id: 'embedding-app-container',
	    html: '<div id="embedding-draw-outer" style="position: relative;">' +
		'<div id="embedding-draw" style="position: absolute; top:0px; left: 0px;">' +
		'</div></div>'
	    }]
    });

    var cellSelectionPanel =  Ext.create('Ext.panel.Panel', {
	id: 'cellselection-app-container',
	layout: 'fit',
	height: '100%',
	width: '100%'
    });

    var geneSelectionPanel =  Ext.create('Ext.panel.Panel', {
	id: 'geneselection-app-container',
	layout: 'fit',
	height: '100%',
	width: '100%',
    });


    var actionsPanel = Ext.create('Ext.panel.Panel', {
	id: 'actions-app-container',
	layout: 'fit',
	height: '100%',
	width: '100%'
    });

    var infoboxPanel = Ext.create('Ext.panel.Panel', {
	layout: 'fit',
	id: 'infoboxPanel',
	title: 'Information',
	padding: 0,
	border: 0,
	header: false,
	height: '100%',
	width: '100%',
	items: [Ext.create('Ext.TabPanel', {
	    layout: 'fit',
	    width: '100%',
	    height: '100%',
	    tabBarPosition: 'top',
	    activeTab: 0,
	    items: [

		{
		    title: 'Cell Selections',
		    glyph: 0xf03a,
		    tooltip: 'View and manage available cell selections',
		    scrollable: true,
		    items: cellSelectionPanel
		},
		{
		    title: 'Actions',
		    id: 'actions-ui-tab',
		    glyph: 0xf0e7,
		    tooltip: 'Run differential expression and other actions between selected sets',
		    items: actionsPanel
		},
				{
		    title: 'Gene Selections',
		    glyph: 0xf03a,
		    tooltip: 'View and manage available gene selections',
		    items: geneSelectionPanel,
		}
	    ],
	    bbar: statusBar
	})] // items of panel
    }); // panel

    // Table viewer
    var tableViewerPanel = Ext.create('Ext.panel.Panel', {
	layout: 'fit',
	id: 'tableExtJSWrapper',
	title: 'Table View',
	header: true, //Show the innermost title
	padding: 0,
	border: 0,
	height: '100%',
	bodyPadding: 0,
	glyph: 0xf0c9,
	items: [ Ext.create('Ext.TabPanel', {
	  	    id: 'tablesTabExtJS',
	    layout: 'fit',
	    width: '100%',
	    height: '100%',
	    tabBarPosition: 'top',
	    items: [
		// {
		//     title: 'Overdispersed Genes',
		//     id: 'odGeneTableViewerExtJS',
		//     layout: 'fit',
		//     height: '100%',
		//     width: '100%',
		//     tooltip: 'This tab displays a table of overdispersed genes',
		//     glyph: 0xf0db,
		// },
		{
		    layout: 'fit',
		    title: 'All Genes',
		    id: 'geneTableViewerExtJS',
		    height: '100%',
		    width: '100%',
		    glyph: 0xf0db,
		    tooltip: 'This tab displays the table of all genes'
		    //items: []
		},
		{
		    title: 'Gene Sets of Interest',
		    id: 'geneSetsOfInterestExtJS',
		    layout: 'border',
		    split: true,
		    height: '100%',
		    width: '100%',
		    tooltip: 'This tab displays the table of predifined gene sets of interest',
		    glyph: 0xf20e,
		    // items: []
		},

		{
		  title: 'Aspects',
		  id: 'aspectsExtJS',
		  layout: 'border',
		  height: '100%',
		  width: '100%',
		  tooltip: 'This tab displays the table of aspects',
		  glyph: 0xf0e8
		},
		{
		  title: 'Differential Expression',
		  id: 'diffExprExtJS',
		  layout: 'border',
		  height: '100%',
		  width: '100%',
		  tooltip: 'Display the results of differential expression',
		  glyph: 0xf07e

		}


	    ] // Items of TabPanel
	   }) // TabPanel
	       ] // items of Panel
    }); // Panel




    // These are the layout panels
    // IMPORTANT NOTE: NOTHING BEYOND THIS FUNCTION SHOULD REFER TO THE PANELS DEFINED BELOW
    // USE THE INNERMOST PANELS ABOVE

    var leftColumnTopPanel = Ext.create('Ext.panel.Panel', {
        region: 'north',
        layout: 'fit',
	header: false,
        id: 'leftColumnTopPanel',
        title: 'leftColumnTopPanel',
        minheight: 100,
        height: '50%',
        bodyPadding: 0,
        split: true,
        items: [ embedding ]
    });


    var leftColumnBottomPanel = Ext.create('Ext.panel.Panel', {
        region: 'center',
        layout: 'fit',
        id: 'leftColumnBottomPanel',
        title: 'leftColumnBottomPanel',
	      header: false,
        height: '50%',
        split: true,
        minHeight: 100,
        collapsible: false,
	items: [infoboxPanel]
    });

   var leftColumnPanel = Ext.create('Ext.panel.Panel',{
        region: 'west',
        id: 'leftColumnPanel',
        collapsible: true,
        split: true,
	      header: false,
        layout: 'border',
        width: '33%',
        heigth: '100%',
        title: 'Embedding and Information View',
        bodyPadding: 0,
        items: [leftColumnTopPanel, leftColumnBottomPanel]
    });

    var centerColumnPanel = Ext.create('Ext.panel.Panel',{
        region: 'center',
        id: 'centreColumnPanel',
	      header: false,
        layout: 'vbox',
        width: '33%',
        title: 'Main Data Viewer',
        items: [
            {
            	id: 'dendrogramPanel',
            	layout: 'fit',
            	width: '100%',
            	height: '10%',
            	title: 'Main View',
            	padding: 0,
            	glyph: 0xf1fe,
            	bodyPadding: 0,
            	split: true,
            	html: '<div id="dendrogram-area-container"></div>'
            },
            {
              id: 'metadataPanel',
              layout: 'fit',
              width: '100%',
              height: '25%',
              padding: 0,
              split: true,
              title: 'Metadata View',
              html: '<div id="metadata-area-container"></div>'
            },
            {
              id: 'aspectPanel',
              layout: 'fit',
              width: '100%',
              height: '10%',
              padding: 0,
              split: true,
              title: 'Aspect View',
              html: '<div id="aspect-heatmap-container"></div>'
            },
            {
              id: 'heatmapPanel',
              layout: 'fit',
              width: '100%',
              height: '55%',
              padding: 0,
              split: true,
              title: 'Heatmap View',
              html: '<div id="heatmap-area-container"></div>'
            }


          ]
    });

    var rightColumnPanel = Ext.create('Ext.panel.Panel', {
        region: 'east',
        id: 'rightColumnPanel',
        collapsible: true,
        layout: 'fit',
        width: '33%',
        title: 'Table View',
	      header: false,
        items: [ tableViewerPanel ]
    });


    // The main Viewport layout
    var viewport = Ext.create('Ext.Viewport', {
        layout: {
            type: 'border',
            padding: 5,
  	    height: '100%',
  	    width: '100%'
        },
        defaults: {
            split: true
        },
        items: [leftColumnPanel,centerColumnPanel,rightColumnPanel]
    });

}

