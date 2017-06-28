function graphViewer(data, plotType){
    this.data = data;
    this.plotType = plotType;
}

graphViewer.prototype.draw = function(canvas){
  if(this.plotType === "scatter"){
    this.drawScatterPlot(canvas);
  }
  else if(this.plotType === "bar"){
    this.drawBarGraph(canvas);
  }
}

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
  }
  
  var boundings = this.measureScatterComponents(ctx, "14px Arial",14,"28px Arial",28,parseInt((canvas).getAttribute("width")),parseInt((canvas).getAttribute("height")), padding, margin, choices, boundries, scaleStyle);
  var realWidth = barSep * 2 + (barWidth + barSep) * this.data.data.length + parseInt(((canvas).getAttribute("width"))) - boundings.plotDim.width;
  if(realWidth < 500){
    realWidth = 500;
    
  }
  (canvas).setAttribute("width", realWidth);
  
  boundings = this.measureScatterComponents(ctx, "14px Arial",14,"28px Arial",28,parseInt((canvas).getAttribute("width")),parseInt((canvas).getAttribute("height")), padding, margin, choices, boundries, scaleStyle);
  
  choices.hasXscale = false;
  this.drawAxisWithScales(ctx, choices, boundings, padding, margin, boundries, scaleStyle);
  
  if(parseInt((canvas).getAttribute("width")) === 500){
    
    var dilation = boundings.plotDim.width/((barSep+barWidth)* this.data.data.length + 2 * barSep);
    barSep = barSep * dilation;
    barWidth = barWidth * dilation;
  }

  for(var i = 0; i < this.data.data.length; i++){
    var barStartY = boundings.plotBR.y;
    var barStartX = boundings.plotTL.x + i * (barSep + barWidth) + barSep;
    var total = 0;
    for(var j = 0; j < this.data.data[i].length; j++){
      total += this.data.data[i][j];
    }
    for(var j = 0; j < this.data.data[i].length; j++){
      if(this.data.data[i][j] !== 0){
        var barHeight = (this.data.data[i][j]/total) * boundings.plotDim.height;
        ctx.fillStyle = this.data.compPalette[j];
        ctx.fillRect(barStartX, barStartY, barWidth, -barHeight);
        barStartY = barStartY - barHeight;
      }
    }
    ctx.fillStyle = 'black';
    ctx.baseline = "top";
    ctx.textAlign = "center";
    ctx.font = boundings.axisFont
    ctx.fillText((i+1) + "" ,barStartX + barWidth/2, boundings.plotBR.y + graphViewer.lineThickness + padding)
  }
}

graphViewer.prototype.drawScatterPlot = function(canvas, xLabel, yLabel, title){
  
  var boundries ={}
  boundries.minX = 0;
  boundries.minY = 0;
  boundries.maxX = 0;
  boundries.maxY = 0;
  for(var i = 0; i < this.data.data.length; i++){
    boundries.minX = Math.min(this.data.data[i][0],boundries.minX);
    boundries.minY = Math.min(this.data.data[i][1],boundries.minY);
    boundries.maxX = Math.max(this.data.data[i][0],boundries.maxX);
    boundries.maxY = Math.max(this.data.data[i][1],boundries.maxY);
  }
  var padding = 8;
  var radius = 4;
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
  for(var i = 0; i < this.data.data.length; i++){
    
    var coordinate = {
      x: (this.data.data[i][0] - boundries.minX)/xRange * boundings.plotDim.width,
      y: (this.data.data[i][1] - boundries.minY)/yRange * boundings.plotDim.height
    }
    ctx.beginPath();
    ctx.arc(coordinate.x + boundings.plotTL.x, boundings.plotBR.y - coordinate.y , radius, 0, 2 * Math.PI, false);
    ctx.fillStyle = 'red';
    ctx.fill();

    ctx.strokeStyle = 'red';
    ctx.stroke();
    
  }
  
}
graphViewer.prototype.measureScatterComponents = function(ctx, axisFont,axisHeight, titleFont, titleHeight, width, height, padding, margin, choices, boundries, scaleStyle){
  var boundings = {}
  boundings.axisFont = axisFont;
  boundings.titleFont = titleFont;
  boundings.xGutter = (choices.hasXscale? axisHeight + padding : 0) + (choices.hasAxisLabels? axisHeight + padding : 0) + margin;
  boundings.yGutter = margin + (choices.hasAxisLabels? axisHeight + padding : 0);
  
  if(choices.hasYscale){
    var step = (boundries.maxY-boundries.minY)/5
    ctx.font = axisFont;
    var maxLength = 0;
    for(var x = boundries.minY; x < boundries.maxY; x += step){maxLength = Math.max(ctx.measureText(scaleStyle(x)).width,maxLength)}
    boundings.yGutter += maxLength + padding;
  }
  
  boundings.topGutter = margin + (choices.hasTitle? titleHeight + padding : 0)
  boundings.rightGutter = margin;
  
  if(choices.hasXscale){
    var step = (boundries.maxX-boundries.minX)/5
    ctx.font = axisFont;
    var maxLength = 0;
    for(var x = boundries.minX; x < boundries.maxX; x += step){maxLength = Math.max(ctx.measureText(scaleStyle(x)).width,maxLength)}
    boundings.rightGutter += maxLength/2;
  }
  
  boundings.plotTL = {
    x: boundings.yGutter + graphViewer.lineThickness,
    y: boundings.topGutter
  }
  boundings.plotBR = {
    x: width - boundings.rightGutter,
    y: height - boundings.xGutter - graphViewer.lineThickness
  }
  boundings.plotDim = {
    width: width - boundings.rightGutter - boundings.yGutter - graphViewer.lineThickness,
    height: height - boundings.topGutter - boundings.xGutter - graphViewer.lineThickness
  }
  boundings.canvasDim = {
    height: height,
    width: width
  }
  
  return boundings;
}
graphViewer.prototype.drawAxisWithScales = function(ctx, choices, boundings, padding, margin, boundries, scaleStyle){
  
  if(choices.hasYscale){
    ctx.textAlign = "right";
    ctx.textBaseline = "middle";
    var scaleSpace = boundings.plotDim.height/5;
    var step = (boundries.maxY - boundries.minY)/5
    for(var i = 0; i < 6; i++){
      ctx.fillStyle = "#000000";
      ctx.fillText(scaleStyle(boundries.minY+(i*step)) + "", (boundings.plotTL.x - graphViewer.lineThickness - padding), boundings.plotBR.y - scaleSpace * i);
      ctx.fillStyle = "#D3D3D3";
      ctx.fillRect(boundings.plotTL.x, boundings.plotBR.y - scaleSpace * i, boundings.plotDim.width, graphViewer.lineThickness);
    }
  }
  
  if(choices.hasXscale){
    ctx.textAlign = "center";
    ctx.textBaseline = "top";
    var scaleSpace = boundings.plotDim.width/5;
    var step = (boundries.maxX-boundries.minX)/5
    for(var i = 0; i < 6; i++){
      ctx.fillStyle = "#000000";
      ctx.fillText(scaleStyle(boundries.minX+(i*step)) + "", (boundings.plotTL.x - graphViewer.lineThickness) + scaleSpace * i, boundings.plotBR.y + padding + graphViewer.lineThickness);
      ctx.fillStyle = "#D3D3D3";
      ctx.fillRect(boundings.plotTL.x + scaleSpace * i, boundings.plotTL.y, -1 * graphViewer.lineThickness, boundings.plotDim.height);
    }
  }
  
  if(choices.hasAxisLabels){

    ctx.fillStyle = "#000000";
    ctx.textAlign = "center";
    ctx.textBaseline = "bottom";
    ctx.fillText(this.data.xLabel, (boundings.plotBR.x - boundings.plotTL.x)/2 + boundings.plotTL.x, boundings.canvasDim.height - margin);

    ctx.textBaseline = "top";
    ctx.rotate(-Math.PI/2);
    ctx.fillText(this.data.yLabel,-((boundings.plotBR.y - boundings.plotTL.y)/2+ boundings.plotTL.y), margin)
    ctx.rotate(Math.PI/2);
  }
  
  if(choices.hasTitle){
    ctx.fillStyle = "#000000";
    ctx.textAlign = "center";
    ctx.textBaseline = "top";
    ctx.font = boundings.titleFont;
    ctx.fillText(this.data.title, (boundings.plotBR.x - boundings.plotTL.x)/2 + boundings.plotTL.x, margin);
    
  }
  
  ctx.fillStyle = "#000000";
  ctx.fillRect(boundings.plotTL.x, boundings.plotTL.y, -1 * graphViewer.lineThickness, boundings.plotDim.height + graphViewer.lineThickness);
  ctx.fillRect(boundings.plotTL.x, boundings.plotBR.y, boundings.plotDim.width, graphViewer.lineThickness);
}
graphViewer.prototype.render = function(){
  var height = 500;
  var width = 500;
  if(this.plotType === "bar"){
    width = Math.max(this.findTrueBarWidth(height,width), 500);
  }
  var graphingImplement = this;
  Ext.create("Ext.window.Window", {
        resizeable: false,
        modal:true,
        items:[
          {
            html:'<canvas id="displayChart" height="'+ height + '" width="'+ width + '"></canvas>'
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
graphViewer.lineThickness = 1;


  

