"use strict";

/*
 * Filename: actionPanelUIcontroller.js
 * Author: Brendan Joyce
 * Date: June 2017
 */

/**
 * Generates custom plots
 * @constructor
 */
function graphViewer(data, plotType){
    this.data = data;
    this.plotType = plotType;

    this.lineThickness = 1;
}

/**
 * Draws a graph onto the target canvas
 * @param {canvas} Target canvas
 */
graphViewer.prototype.draw = function(canvas){
  if(this.plotType === "scatter"){
    this.drawScatterPlot(canvas);
  }
  else if(this.plotType === "bar"){
    this.drawBarGraph(canvas);
  }
}

/**
 * Draws a percent composition bar graph onto the target canvas
 * @param {canvas} Target canvas
 */
graphViewer.prototype.drawBarGraph = function(canvas){
  var boundries = {
    minY: 0,
    maxY: 100,
    minX: 0,
    maxX: this.data.data.length
  }
  var barSep = 5;
  var barWidth = 16;
  var padding = 8;
  var margin = 5;
  var scaleStyle = function(x){return (Math.round(x) + "%")}
  var ctx = canvas.getContext('2d');
  var choices = {
    hasXscale: true,
    hasYscale: true,
    hasAxisLabels: true,
    hasTitle: true,
  }// choices represents an object that contains information about x and y scale, title, and axis labels existance,
  var hoverFields = [];// x range by y range

  var boundings = this.measureScatterComponents(ctx, "14px Arial",14,"28px Arial",28,
    parseInt((canvas).getAttribute("width")),parseInt((canvas).getAttribute("height")),
    padding, margin, choices, boundries, scaleStyle);

  var realWidth = barSep * 2 + (barWidth + barSep) * this.data.data.length + parseInt(((canvas).getAttribute("width"))) - boundings.plotDim.width;

  //sets realwidth to mininum of 500
  if(realWidth < 500){
    realWidth = 500;

  }
  (canvas).setAttribute("width", realWidth);

  boundings = this.measureScatterComponents(ctx, "14px Arial",14,"28px Arial",
    28,parseInt((canvas).getAttribute("width")),parseInt((canvas).getAttribute("height")),
    padding, margin, choices, boundries, scaleStyle);

  choices.hasXscale = false;
  this.drawAxisWithScales(ctx, choices, boundings, padding, margin, boundries, scaleStyle);

  if(parseInt((canvas).getAttribute("width")) === 500){
    var dilation = boundings.plotDim.width/((barSep+barWidth)* this.data.data.length + 2 * barSep);
    barSep = barSep * dilation;
    barWidth = barWidth * dilation;
  }

  //for each value in the array of clusters fill a multiple boxes representing membership with other clusters
  for(var i = 0; i < this.data.data.length; i++){
    var barStartY = boundings.plotBR.y;
    var barStartX = boundings.plotTL.x + i * (barSep + barWidth) + barSep;

    hoverFields.push({xStart: barStartX, yRanges: []});

    var total = 0;
    for(var j = 0; j < this.data.data[i].length; j++){
      total += this.data.data[i][j];
    }
    for(var j = 0; j < this.data.data[i].length; j++){
      if(this.data.data[i][j] !== 0){
        var barHeight = (this.data.data[i][j]/total) * boundings.plotDim.height;

        ctx.fillStyle = this.data.compPalette[j].substring(0,7);
        ctx.fillRect(barStartX, barStartY, barWidth, -barHeight);
        hoverFields[i].yRanges.push({start: barStartY, height: barHeight, value: j});

        barStartY = barStartY - barHeight;
      }
    }
    ctx.fillStyle = 'black';
    ctx.baseline = "top";
    ctx.textAlign = "center";
    ctx.font = boundings.axisFont
    ctx.fillText((i+1) + "" ,barStartX + barWidth/2, boundings.plotBR.y + this.lineThickness + padding)
  }

  //simple tooltip generator that displays cluster name when hovering over a give point
  function showTooltip(x, y, contents) {
    $('<div id="barplot_tooltip">' + contents + '</div>').css({
        position: 'absolute',
        display: 'none',
        top: y + 5,
        left: x + 5,
        border: '1px solid #ffdddd',
        padding: '2px',
        'background-color': '#ffeeee',
        opacity: 0.80
    }).appendTo(".canvas_holder").fadeIn(0);
}

  var parent = this;
  document.getElementById("displayChart").addEventListener("mousemove",function(event){
    var xPos = event.offsetX;
    var yPos = event.offsetY;
    var xPlotPos = Math.floor((xPos - boundings.plotTL.x)/(barWidth+barSep));

    //finds the tooltip's information if thecursor is hovering over a valid hover field
    if(xPlotPos >= 0 && xPlotPos < hoverFields.length && hoverFields[xPlotPos].xStart <= xPos){
      for(var i = 0; i < hoverFields[xPlotPos].yRanges.length; i++){
        if(hoverFields[xPlotPos].yRanges[i].start > yPos && hoverFields[xPlotPos].yRanges[i].start - hoverFields[xPlotPos].yRanges[i].height < yPos){
          $("#barplot_tooltip").remove();
          showTooltip(xPos,yPos,"Cluster index: " + (hoverFields[xPlotPos].yRanges[i].value + 1) + " Color: " + parent.data.compPalette[hoverFields[xPlotPos].yRanges[i].value].substring(0,7))
          return;
        }
      }
    }

    $("#barplot_tooltip").remove();
  })
}
/**
 * Draws a scatter plot with provided data xLabel, yLabel, title, and canvas target
 * @param {canvas} Canvas as target for drawing the new plot
 * @param {xLabel} X axis title
 * @param {yLabel} Y axis title
 * @param {title} Title fo the graph
 */
graphViewer.prototype.drawScatterPlot = function(canvas, xLabel, yLabel, title){

  var boundries ={}
  boundries.minX = 0;
  boundries.minY = 0;
  boundries.maxX = 0;
  boundries.maxY = 0;

  //find minumums and maximums of the data set
  for(var i = 0; i < this.data.data.length; i++){
    boundries.minX = Math.min(this.data.data[i][0],boundries.minX);
    boundries.minY = Math.min(this.data.data[i][1],boundries.minY);
    boundries.maxX = Math.max(this.data.data[i][0],boundries.maxX);
    boundries.maxY = Math.max(this.data.data[i][1],boundries.maxY);
  }
  var padding = 8;
  var radius = 2;
  var margin = 5;
  var ctx = canvas.getContext('2d');
  var choices = {
    hasXscale: true,
    hasYscale: true,
    hasAxisLabels: true,
    hasTitle: true,
  }

  var scaleFunction = function(x){return (x.toFixed(2) + "")};
  var boundings = this.measureScatterComponents(ctx, "14px Arial",14,"28px Arial",28,parseInt((canvas).getAttribute("width")),parseInt((canvas).getAttribute("height")), padding, margin, choices, boundries,scaleFunction);


  this.drawAxisWithScales(ctx, choices,boundings, padding ,margin, boundries, function(x){return (Math.round(x * 100)/100)});
  var xRange = boundries.maxX-boundries.minX;
  var yRange = boundries.maxY-boundries.minY;

  //plot the points on the graph
  for(var i = 0; i < this.data.data.length; i++){
    var coordinate = {
      x: (this.data.data[i][0] - boundries.minX)/xRange * boundings.plotDim.width,
      y: (this.data.data[i][1] - boundries.minY)/yRange * boundings.plotDim.height
    }
    ctx.beginPath();
    ctx.arc(coordinate.x + boundings.plotTL.x, boundings.plotBR.y - coordinate.y , radius, 0, 2 * Math.PI, false);
    ctx.fillStyle = 'black';
    ctx.fill();

    ctx.strokeStyle = 'black';
    ctx.stroke();

  }

}

/** Measure the components for a scatter canvas and determine how much padding should be distrbuted
 * @param {ctx} 2d Context from the canvas being used
 * @param {axisFont} Font for axis titles
 * @param {axisHeight} Font size for axis
 * @param {titleFont} Font for title
 * @param {titleHeight} Font size for title height
 * @param {width} Width of the plot
 * @param {height} Height of the plot
 * @param {padding} desired padding on the graph's content
 * @param {margin} Margin surrounding the graph on the canvas
 * @param {choices} Object consisting of parameters specifying whether or not certain components should be included
 * @param {boundries} Boundries that enclose the graph
 * @param {scaleStyle} Means of formatting scale to create a reasonable graph
 */
graphViewer.prototype.measureScatterComponents = function(ctx, axisFont,axisHeight, titleFont, titleHeight, width, height, padding, margin, choices, boundries, scaleStyle){
  var boundings = {}
  boundings.axisFont = axisFont;
  boundings.titleFont = titleFont;
  boundings.xGutter = (choices.hasXscale? axisHeight + padding : 0) + (choices.hasAxisLabels? axisHeight + padding : 0) + margin;
  boundings.yGutter = margin + (choices.hasAxisLabels? axisHeight + padding : 0);

  //format adjustments for inclusion of yscale
  if(choices.hasYscale){
    var step = (boundries.maxY-boundries.minY)/5
    ctx.font = axisFont;
    var maxLength = 0;
    for(var x = boundries.minY; x < boundries.maxY; x += step){maxLength = Math.max(ctx.measureText(scaleStyle(x)).width,maxLength)}
    boundings.yGutter += maxLength + padding;
  }

  boundings.topGutter = margin + (choices.hasTitle? titleHeight + padding : 0)
  boundings.rightGutter = margin;

  //format adjustments for inclusion of xscale
  if(choices.hasXscale){
    var step = (boundries.maxX-boundries.minX)/5
    ctx.font = axisFont;
    var maxLength = 0;
    for(var x = boundries.minX; x < boundries.maxX; x += step){maxLength = Math.max(ctx.measureText(scaleStyle(x)).width,maxLength)}
    boundings.rightGutter += maxLength/2;
  }

  //coordinates and dimensions of plot and canvas
  boundings.plotTL = {
    x: boundings.yGutter + this.lineThickness,
    y: boundings.topGutter
  }
  boundings.plotBR = {
    x: width - boundings.rightGutter,
    y: height - boundings.xGutter - this.lineThickness
  }
  boundings.plotDim = {
    width: width - boundings.rightGutter - boundings.yGutter - this.lineThickness,
    height: height - boundings.topGutter - boundings.xGutter - this.lineThickness
  }
  boundings.canvasDim = {
    height: height,
    width: width
  }

  return boundings;
}
/**
 * Draw the axis and scales based on the user provided boundings and canvas
 * @param {ctx} 2D context for the canvas being drawn to
 * @param {choices} Object consisting of parameters specifying whether or not certain components should be included
 * @param {boundries} Boundries that enclose the graph
 * @param {padding} desired padding on the graph's content
 * @param {margin} Margin surrounding the graph on the canvas
 * @param {boundries} Boundries that enclose the graph
 * @param {scaleStyle} Means of formatting scale to create a reasonable graph
 */
graphViewer.prototype.drawAxisWithScales = function(ctx, choices, boundings, padding, margin, boundries, scaleStyle){

  //adds yscale if its existence is specified
  if(choices.hasYscale){
    ctx.textAlign = "right";
    ctx.textBaseline = "middle";
    var scaleSpace = boundings.plotDim.height/5;
    var step = (boundries.maxY - boundries.minY)/5
    //draw xScale line by line
    for(var i = 0; i < 6; i++){
      ctx.fillStyle = "#000000";
      ctx.fillText(scaleStyle(boundries.minY+(i*step)) + "", (boundings.plotTL.x - this.lineThickness - padding), boundings.plotBR.y - scaleSpace * i);
      ctx.fillStyle = "#D3D3D3";
      ctx.fillRect(boundings.plotTL.x, boundings.plotBR.y - scaleSpace * i, boundings.plotDim.width, this.lineThickness);
    }
  }

  //adds xscale if its existence is specified
  if(choices.hasXscale){
    ctx.textAlign = "center";
    ctx.textBaseline = "top";
    var scaleSpace = boundings.plotDim.width/5;
    var step = (boundries.maxX-boundries.minX)/5;
    //draw xScale line by line
    for(var i = 0; i < 6; i++){
      ctx.fillStyle = "#000000";
      ctx.fillText(scaleStyle(boundries.minX+(i*step)) + "", (boundings.plotTL.x - this.lineThickness) + scaleSpace * i, boundings.plotBR.y + padding + this.lineThickness);
      ctx.fillStyle = "#D3D3D3";
      ctx.fillRect(boundings.plotTL.x + scaleSpace * i, boundings.plotTL.y, -1 * this.lineThickness, boundings.plotDim.height);
    }
  }

  //adds axis labels if they exist
  if(choices.hasAxisLabels){

    ctx.fillStyle = "#000000";
    ctx.textAlign = "center";
    ctx.textBaseline = "bottom";
    ctx.fillText(this.data.xLabel, (boundings.plotBR.x - boundings.plotTL.x)/2 + boundings.plotTL.x, boundings.canvasDim.height - margin);

    //have to rotate the context in order to print y axis label
    ctx.textBaseline = "top";
    ctx.rotate(-Math.PI/2);
    ctx.fillText(this.data.yLabel,-((boundings.plotBR.y - boundings.plotTL.y)/2+ boundings.plotTL.y), margin)
    ctx.rotate(Math.PI/2);
  }

  //adds a title if it is specified
  if(choices.hasTitle){
    ctx.fillStyle = "#000000";
    ctx.textAlign = "center";
    ctx.textBaseline = "top";
    ctx.font = boundings.titleFont;
    ctx.fillText(this.data.title, (boundings.plotBR.x - boundings.plotTL.x)/2 + boundings.plotTL.x, margin);

  }

  ctx.fillStyle = "#000000";
  ctx.fillRect(boundings.plotTL.x, boundings.plotTL.y, -1 * this.lineThickness, boundings.plotDim.height + this.lineThickness);
  ctx.fillRect(boundings.plotTL.x, boundings.plotBR.y, boundings.plotDim.width, this.lineThickness);
}

/**
 * Contoller for the functionality of the graph viewer
 * Renders the window for viewing and saving elements from the graph
 */
graphViewer.prototype.render = function(){
  var height = 500;
  var width = 500;


  if(this.plotType === "bar"){
    width = Math.max(this.findTrueBarWidth(height,width), 500);
  }
  var graphingImplement = this;

  //Viewing box + save button
  Ext.create("Ext.window.Window", {
        resizeable: false,
        modal:true,
        items:[
          {
            html:'<div class="canvas_holder"><canvas id="displayChart" height="'+ height + '" width="'+ width + '"></canvas></div>'
          },
          {
            xtype: "button",
            text: "Save Image",
            handler: function(){
              pagHelpers.regC(25);
              var canvas = document.createElement('canvas');
              canvas.height = height;
              canvas.width = width;
              graphingImplement.draw(canvas)
              var imageName = "image.png"
              if(graphingImplement.chatType === "bar"){
                imageName = "matrixComp.png"
              }
              else if(graphingImplement.chartType === "scatter"){
                imageName = "diffExpr.png"
              }
              const maxSize = 2000;
              if (canvas.width > maxSize | canvas.height >maxSize){
                Ext.Msg.show({
                  title: 'Warning',
                      msg: 'The current canvas size exceeds ' + maxSize + 'px in at least one dimention.' +
                       'This may cause problems during exporting. Do you want to continue?',
                       buttons: Ext.Msg.OKCANCEL,
                       fn: function(s) {
                         if (s == 'ok') {
                            canvas.toBlob(function(data){pagHelpers.downloadURL(data, imageName, canvas)})
                         } //if
                       } //fn
                    }) // Ext.Msg.show
                } else {
                          canvas.toBlob(function(data){pagHelpers.downloadURL(data, imageName, canvas)})
                }
            }
          },
        ],
  }).show();
  this.draw(document.getElementById("displayChart"));
}

/**
 * Preprocessing to determine the actual width of the plot if it contains a bar graph
 * @param {height} Estimated height of canvas
 * @param {width} Estimated Width of canvas
 * @returns Actual width of the bargraph as integer
 */
graphViewer.prototype.findTrueBarWidth = function(height, width){
  var boundries = {
    minY: 0,
    maxY: 100,
    minX: 0,
    maxX: this.data.data.length
  }
  var barSep = 5;
  var barWidth = 16;
  var padding = 8;
  var margin = 5;
  var scaleStyle = function(x){return (Math.round(x) + "%")}
  var ctx = document.createElement("canvas").getContext('2d');
  var choices = {
    hasXscale: true,
    hasYscale: true,
    hasAxisLabels: true,
    hasTitle: true,
  }

  var boundings = this.measureScatterComponents(ctx, "14px Arial",14,"28px Arial",28, width, height, padding, margin, choices, boundries, scaleStyle);
  var realWidth = barSep * 2 + (barWidth + barSep) * this.data.data.length + width - boundings.plotDim.width;
  return realWidth;
}







