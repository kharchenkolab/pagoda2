/*
 * Filename: embeddingViewerScatterD3.js
 * Author: Nikolas Barkas
 * Date: March 2017
 * Description implements scatter plot embedding for pagoda2 
 * using d3
 */

/**
 * Implements scatter plot embedding for pagoda2 using D3 SVG
 * plotting
 * @constructor
 */
function scatterEmbeddingD3() {
    console.log('Initialising scatterEmbeddingD3');

    // Here we need to tap into the extjs container
    // component resize function 
    var element = Ext.getCmp('embedding-app-container');
    element.onResize = function(){
	embView = new embeddingViewer();
	embView.redraw();
    };


    // TODO: Move this to the main embedding drawing object
    var evtBus = new eventBus();
    // Dendrogram node has been clicked
    // Set the coloring scheme to dendrogram ( which uses the selection controller )
    // and call updateColors
    evtBus.register("dendrogram-node-click", null, function(e,p) {
	var embV = new embeddingViewer();

	// Update  configuration of embedding
	// The actual selected node is passed by the cell selection
	embV.setColorConfiguration('dendrogram');
	embV.setDendrogramColorInfo({nodeType: p.type});

	embV.updateColors();
    }); 
}

scatterEmbeddingD3.prototype.updateColorsGeneexpression = function() {
    throw new Error('updateColorsGeneexpression not implemented');
}

/**
 * Update the embedding colors to match metadata information
 */
scatterEmbeddingD3.prototype.updateColorsMetadata = function() {
    // Get the configuration information that we need
    var ev = new embeddingViewer();
    var config = ev.getConfig();


    var dataCntr = new dataController();

    // Get the embedding viewer config and the name
    // of the metadata we want to plot
    var metadataColorInfo = config.metaDataColorInfo;
    var metadataName = metadataColorInfo.metadataName;

    // FIXME: cellSelection on the data controller for the metadata
    // doesn't work so we are passing null here. We want to move
    // towards a model where we don't specify cells at all if we 
    // don't want anything other than everything in the default order
    // TODO: We would like to minimize movement of data 
    // so we should have the ability to specify which metadata we want
    // and  only get that 
    dataCntr.getCellMetadata(function(data) {
	colorData = data[metadataName].data;
	colorPalette = data[metadataName].palette;

	// Prep data for d3
	var d3data =[];
	for (i in colorData) {
	    var clusterId = Number(colorData[i]);
	    var clusterColor = colorPalette[clusterId-1];
	    // Remove the alpha channel
	    clusterColor = clusterColor.substring(0,7);
	    d3data[d3data.length] = [i, clusterId ,clusterColor];
	 }

	// Update the colors
	var embV = new embeddingViewer();
	var alpha = embV.getCurrentAlpha();

	d3.select('#embedding-draw')
	    .selectAll('circle')
	    .data(d3data)
	    .attr('opacity', alpha)
	    .attr('fill', function(d) {
		return d[2];
	    });
    });
}

scatterEmbeddingD3.prototype.plotEmbedding = function() {
    var dataCntr = new dataController();

    // Get the configuration information that we need
    var ev = new embeddingViewer();
    var config = ev.getConfig();

    var type = config["type"];
    var embeddingType = config["embeddingType"];

    dataCntr.getEmbedding(type,embeddingType, function(data){
	// TODO: embeddings must have type to implement things other
	// than scatter plots, this is best implemented as separate classes
	// e.g. scatterPlotEmbedder with some kind of virtual function mechanism

	plotData = pagHelpers.jsonSerialisedToArrayOfArrays(data);

	// Clean-up existing
	var embeddingContainer = $("#embedding-draw");
	embeddingContainer.empty();

	// TODO: Check if the event handlers need cleaning - potential mem leak
	var extJsContainer = Ext.getCmp('embeddingExtJSWrapper');
	var plotHeight = extJsContainer.body.getHeight(true);
	var plotWidth = extJsContainer.body.getWidth(true);

	// Which dim is limiting?
	size = Math.min(plotWidth, plotHeight);

	var svg = d3.select("#embedding-draw")
	    .append("svg")
	    .attr("width", size)
	    .attr("height", size);

	// Center it in x and y
	var vpad = 0;
	var hpad = 0;

	if (plotHeight < plotWidth) {
	    hpad = (plotWidth - size)/2
	} else {
	    vpad = (plotHeight - size) /2
	}
	
	// For getting the plot size
	var embViewer =  new embeddingViewer();

	$('#embedding-draw > svg').css('margin-top', vpad);
	$('#embedding-draw > svg').css('margin-left', hpad);

	// Slight shrink the window scale to avoid plotting on the edges
	var rangeScaleFactor = 0.95;

	var xScale =  d3.scaleLinear()
	    .domain([d3.min(plotData, function(d) { return d[1]; }), d3.max(plotData, function(d) { return d[1]; })])
	    .range([(size * (1 - rangeScaleFactor)), size * rangeScaleFactor]);

	var yScale = d3.scaleLinear()
	    .domain([d3.min(plotData, function(d) { return d[2]; }), d3.max(plotData, function(d) { return d[2]; })])
	    .range([(size * (1 - rangeScaleFactor)), size * rangeScaleFactor]);

	// The size to plot at
	var pointsize = embViewer.getCurrentPointSize();
	var alpha = embViewer.getCurrentAlpha();
	
	// The border
	var stroke = '';
	var strokeWidth = '1';
	if (embViewer.getCurrentBorder() === true ) {
	    stroke = embViewer.getCurrentBorderColor();
	    strokeWidth = embViewer.getCurrentBorderWidth();
	}

	// Plot the datapoints
	svg.selectAll("circle")
	    .data(plotData)
	    .enter()
	    .append("circle")
	    .attr("cx", function(d){return xScale(d[1]) })
	    .attr("cy",function(d){return yScale(d[2]) })
	    .attr("r", pointsize)
	    .attr("opacity", alpha)
	    .attr("stroke", stroke)
	    .attr("stroke-width", strokeWidth)
	    .attr("id", function(d) { return "plotpoint_" + d[0] })
	    .on("mouseover",  function() {
		// Generate event on eventBus
		var eB = new eventBus();
		
		// Remove the d3 point id prefix
		var re = new RegExp('^plotpoint_');
		var cellId = this.id;
		cellId.replace(re, '');

		eB.publish("embedding-mouseover-plotpoint", {cellId: this.id});
		
	    })
	    .on("click", function() {
		// Generate event on eventBus
		var eB =  new eventBus();
		eB.publish("embedding-click-plotpoint", {cellId: this.id});
	    });
    });

    // Update the colors to what is currently selected
    this.updateColors();
} // embeddingViewer.prototype.plotEmbedding

/**  
 * Update the colors -- this embedder does not need to redraw
 */
scatterEmbeddingD3.prototype.updateColors = function() {
    var ev = new embeddingViewer();
    var config = ev.getConfig();

    var colconfig = config.colors;

    if (typeof colconfig === 'undefined') return;

    if( colconfig  === "dendrogram") {
	this.updateColorsDendrogram();
    } else if ( colconfig === 'metadata') {
	this.updateColorsMetadata();
    } else if ( colconfig === 'geneexpression') {
	this.updateColorsGeneexpression();
    } else {
	throw new Error('Unknown embedding color setting: ' + colconfig );
    }
}

/**
 * Clean up the DOM and any other resources used
 * so that a new embedding can be plotted
 */
scatterEmbeddingD3.prototype.cleanup = function() {
    throw new Error('Not implemeted');
}

/**
 * Update the colors using dendrogram information 
 */
scatterEmbeddingD3.prototype.updateColorsDendrogram = function() {
        var embV = new embeddingViewer();
        var dendV = new dendrogramViewer();

	if (embV.currentConfiguration.dendrogramColorInfo.nodeType == "vertical") {
	    var dataCntr = new dataController();
	    var selCntr =  new cellSelectionController();
	    var embV = new embeddingViewer();
	    var type= embV.currentConfiguration["type"];
	    var embeddingType =  embV.currentConfiguration["embeddingType"];
	    var curPrimSel = selCntr.getSelection("currentPrimarySel");

	    dataCntr.getEmbedding(type, embeddingType, function(data) {
		plotData = pagHelpers.jsonSerialisedToArrayOfArrays(data);
		// Amend the data according to the cell selection
		for (var i = 0; i < plotData.length; i++) {
		    var group;

		    if ($.inArray(plotData[i][0], curPrimSel) > -1) {
			plotData[i][3] = "selected";
		    } else {
			plotData[i][3] = "notselected";
		    }
		}

		var alpha = embV.getCurrentAlpha();
		// Update d3
		d3.select("#embedding-draw")
		    .selectAll("circle")
		    .data(plotData)
		    .attr("opacity", alpha)
		    .attr("fill", function(d){ 
			if (d[3] === "selected") {
			    return p2globalParams.dendrogram.selectionColors.selectedMain;
			};
			return p2globalParams.dendrogram.selectionColors.deselected;
		    });
	    });
	} else if (embV.currentConfiguration.dendrogramColorInfo.nodeType == "horizontal") {
	    var dataCntr = new dataController();
	    var selCntr =  new cellSelectionController();
	    var embV = new embeddingViewer();
	    var type= embV.currentConfiguration["type"];
	    var embeddingType =  embV.currentConfiguration["embeddingType"];

	    dataCntr.getEmbedding(type, embeddingType, function(data) {
		plotData = pagHelpers.jsonSerialisedToArrayOfArrays(data);

		    var curPrimSel = selCntr.getSelection("currentPrimarySel");
		    var curAltSel = selCntr.getSelection("currentAltSel");

		// Amend the data according to the cell selection
		for (var i = 0; i < plotData.length; i++) {
		    var group;

		    if ($.inArray(plotData[i][0], curPrimSel) > -1) {
			plotData[i][3] = "left";
		    } else if ($.inArray(plotData[i][0], curAltSel) > -1) {
			plotData[i][3] = "right";
		    } else {
			plotData[i][3] = "notselected";
		    }
		}

		var alpha = embV.getCurrentAlpha();
		d3.select("#embedding-draw")
		    .selectAll("circle")
		    .data(plotData)
		    .attr("opacity", alpha)
		    .attr("fill", function(d){ 
			if (d[3] === "left") {
			    return p2globalParams.dendrogram.selectionColors.selectedMain;
			} else if (d[3] === "right") {
			    return p2globalParams.dendrogram.selectionColors.selectedAlt;
			}
			return p2globalParams.dendrogram.selectionColors.deselected;
		    }); // d3.select
	    });
	} else { 
	    console.error("Unknown click type");
        }; // if (p.type == "vertical")
}


