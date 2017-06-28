function graphViewer(data){
      this.data = data;
}

graphViewer.prototype.drawScatterPlot = function(canvas, xLabel, yLabel, title){
  
  var boundries ={}
  boundries.minX = 0;
  boundries.minY = 0;
  boundries.maxX = 0;
  boundries.maxY = 0;
  for(var i = 0; i < this.data.length; i++){
    boundries.minX = Math.min(this.data[i][0],boundries.minX);
    boundries.minY = Math.min(this.data[i][1],boundries.minY);
    boundries.maxX = Math.max(this.data[i][0],boundries.maxX);
    boundries.maxY = Math.max(this.data[i][1],boundries.maxY);
  }
  var padding = 8;
  var radius = 4;
  var margin = 5;
  var ctx = document.getElementById(canvas).getContext('2d');
  var choices = {
    hasXscale: true,
    hasYscale: true,
    hasAxisLabels: true,
    hasTitle: true,
  }
  
  var boundings = this.measureScatterComponents(ctx, "14px Arial",14,"28px Arial",28,parseInt(document.getElementById(canvas).getAttribute("width")),parseInt(document.getElementById(canvas).getAttribute("height")), padding, margin, choices, boundries);
  
  this.drawAxisWithScales(ctx, choices,boundings, xLabel ,yLabel ,title , padding ,margin, boundries)
  var xRange = boundries.maxX-boundries.minX;
  var yRange = boundries.maxY-boundries.minY;
  for(var i = 0; i < this.data.length; i++){
    
    var coordinate = {
      x: (this.data[i][0] - boundries.minX)/xRange * boundings.plotDim.width,
      y: (this.data[i][1] - boundries.minY)/yRange * boundings.plotDim.height
    }
    ctx.beginPath();
    ctx.arc(coordinate.x + boundings.plotTL.x, boundings.plotBR.y - coordinate.y , radius, 0, 2 * Math.PI, false);
    ctx.fillStyle = 'red';
    ctx.fill();

    ctx.strokeStyle = 'red';
    ctx.stroke();
    
  }
  
}
graphViewer.prototype.measureScatterComponents = function(ctx, axisFont,axisHeight, titleFont, titleHeight, width, height, padding, margin, choices, boundries){
  var boundings = {}
  boundings.axisFont = axisFont;
  boundings.titleFont = titleFont;
  boundings.xGutter = (choices.hasXscale? axisHeight + padding : 0) + (choices.hasAxisLabels? axisHeight + padding : 0) + margin;
  boundings.yGutter = margin + (choices.hasAxisLabels? axisHeight + padding : 0);
  
  if(choices.hasYscale){
    var step = (boundries.maxY-boundries.minY)/5
    ctx.font = axisFont;
    var maxLength = 0;
    for(var x = boundries.minY; x < boundries.maxY; x += step){maxLength = Math.max(ctx.measureText(x.toFixed(2) + "").width,maxLength)}
    boundings.yGutter += maxLength + padding;
  }
  
  boundings.topGutter = margin + (choices.hasTitle? titleHeight + padding : 0)
  boundings.rightGutter = margin;
  
  if(choices.hasXscale){
    var step = (boundries.maxX-boundries.minX)/5
    ctx.font = axisFont;
    var maxLength = 0;
    for(var x = boundries.minX; x < boundries.maxX; x += step){maxLength = Math.max(ctx.measureText(x.toFixed(2) + "").width,maxLength)}
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
graphViewer.prototype.drawAxisWithScales = function(ctx, choices, boundings, xLabel, yLabel, title, padding, margin, boundries){
  
  if(choices.hasYscale){
    ctx.textAlign = "right";
    ctx.textBaseline = "middle";
    var scaleSpace = boundings.plotDim.height/5;
    var step = (boundries.maxY-boundries.minY)/5
    for(var i = 0; i < 6; i++){
      ctx.fillStyle = "#000000";
      ctx.fillText((Math.round((boundries.minY+(i*step))*100) / 100) + "", (boundings.plotTL.x - graphViewer.lineThickness - padding), boundings.plotBR.y - scaleSpace * i);
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
      ctx.fillText((Math.round((boundries.minX+(i*step))*100) / 100) + "", (boundings.plotTL.x - graphViewer.lineThickness) + scaleSpace * i, boundings.plotBR.y + padding + graphViewer.lineThickness);
      ctx.fillStyle = "#D3D3D3";
      ctx.fillRect(boundings.plotTL.x + scaleSpace * i, boundings.plotTL.y, -1 * graphViewer.lineThickness, boundings.plotDim.height);
    }
  }
  
  if(choices.hasAxisLabels){

    ctx.fillStyle = "#000000";
    ctx.textAlign = "center";
    ctx.textBaseline = "bottom";
    ctx.fillText(xLabel, (boundings.plotBR.x - boundings.plotTL.x)/2 + boundings.plotTL.x, boundings.canvasDim.height - margin);

    ctx.textBaseline = "top";
    ctx.rotate(-Math.PI/2);
    ctx.fillText(yLabel,-((boundings.plotBR.y - boundings.plotTL.y)/2+ boundings.plotTL.y), margin)
    ctx.rotate(Math.PI/2);
  }
  
  if(choices.hasTitle){
    ctx.fillStyle = "#000000";
    ctx.textAlign = "center";
    ctx.textBaseline = "top";
    ctx.font = boundings.titleFont;
    ctx.fillText(title, (boundings.plotBR.x - boundings.plotTL.x)/2 + boundings.plotTL.x, margin);
    
  }
  
  ctx.fillStyle = "#000000";
  ctx.fillRect(boundings.plotTL.x, boundings.plotTL.y, -1 * graphViewer.lineThickness, boundings.plotDim.height + graphViewer.lineThickness);
  ctx.fillRect(boundings.plotTL.x, boundings.plotBR.y, boundings.plotDim.width, graphViewer.lineThickness);
}
graphViewer.lineThickness = 1;

