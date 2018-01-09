"use strict";

/**
 * Track the completion of specific events during app loading
 * to enable post-loading event triggering
 * @constructor
 */
function LoadingProgressTracker() {
  if (typeof LoadingProgressTracker.instance === 'object') {
	  return LoadingProgressTracker.instance;
  };
  
  var eB = new eventBus();
  
  // When loading is complete display entire gene table in heatmap
  eB.register('loadComplete',{}, function(e,p){
        // The gene table from the gene table viewer
        var geneTable = Ext.getCmp('extjsgenetable');
        //geneTable.getSelectionModel().selectRange(0,50);
        if (typeof geneTable !== 'undefined') {
          geneTable.getSelectionModel().selectAll();
        }
  })

  // We are only tracking 3 events here, do it manually
  this.eventsTrack = Array(false,false,false);
  eB.register('initialMetadataLoadComplete',{},function(e,p){
    LoadingProgressTracker.instance.eventsTrack[0] = true;
    LoadingProgressTracker.instance.checkEvents();
  });
  
  eB.register('initialEmbeddingPlotComplete',{},function(e,p){
    LoadingProgressTracker.instance.eventsTrack[1] = true;
    LoadingProgressTracker.instance.checkEvents();
  });
  
  // There is difficulty implementing this because of many level callback issues
  /*
  eB.register('initialAspectHeatmapLoadComplete',{}, function(e,p){
    console.log('asdf: initialAspectHeatmapLoadComplete');
    LoadingProgressTracker.instance.eventsTrack[2] = true;
    LoadingProgressTracker.instance.checkEvents();
  });
  */
  
  LoadingProgressTracker.instance = this;
};

LoadingProgressTracker.prototype.checkEvents = function() {
  //debugger;
  
  var done = true;
  for (var i = 0; i < 2; i++) {
    if (this.eventsTrack[i] === false) {
      done = false;
    }
  }
  
  if (done) {
    var eb = new eventBus();
    eb.publish('loadComplete');
  }
}