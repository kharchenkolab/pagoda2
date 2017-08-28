"use strict";

self.collectedResults = [];
self.geneName = null;
self.selAidx = null;
self.selBidx = null;
self.method = null;
self.chunks = 50;
self.minchunksize = 500;

// Main event listener for the worker thread
self.addEventListener("message", function(e){
  var callParams = e.data;

  if(callParams.type === "initiate"){
    handleInitiateCommand(e);
  } else if(callParams.type === "process"){
    handleProcessCommand(e);
  }

},false);

function handleInitiateCommand(e) {
    self.geneNames = e.data.params.geneNames;
    self.method = e.data.method;
    e.data.params.geneNames = null;

    var callParams = e.data;

    callParams.params.step = Math.max(Math.floor(self.geneNames.length/self.chunks),self.minchunksize);
    callParams.params.index = 0;
    callParams.params.numCells = callParams.data.length;

    //if there is only one selection given make the second selection off of the indexes of the cellOrderData keys
    if(callParams.selections.length === 1){
      // Only one selection
      self.selAidx = [];
      self.selBidx = [...callParams.data.keys()];
      //creates array of cell indexes based on their corresponding index in cell order array
      for(var i = 0; i < callParams.selections[0].length; i++){
        var idx = callParams.data.indexOf(callParams.selections[0][i]);
        if(idx !== -1){
          self.selAidx.push(idx);
        }
      }
    } else if(callParams.selections.length === 2){
      // Two selections
      self.selAidx = [];
      self.selBidx = [];
      for(var i = 0; i < callParams.selections[0].length; i++){
        var idx = callParams.data.indexOf(callParams.selections[0][i]);
        if(idx !== -1){
          self.selAidx.push(idx);
        }
      }
      for(var i = 0; i < callParams.selections[1].length; i++){
        var idx = callParams.data.indexOf(callParams.selections[1][i]);
        if(idx !== -1){
          self.selBidx.push(idx);
        }
      }
    }

    var nextSliceGenes = self.geneNames.slice(
          callParams.params.index,
          Math.min(callParams.params.index + callParams.params.step,self.geneNames.length)
    );

    postMessage({
        type: "expr vals",
        data: nextSliceGenes,
      params: callParams.params
    });
}

function handleProcessCommand(e) {
  var callParams = e.data;
  if(self.method === "wilcoxon"){
    runWilcoxonOnGroup(callParams.params, callParams.data);
  } else {
    // TODO: Handle error
    console.log('Unknown method')
  }

  //advance index to current spot
  callParams.params.index += callParams.params.step;

  //continue requesting data if data still needs to be read
  if(callParams.params.index < self.geneNames.length){
    var nextGeneNames = self.geneNames.slice(callParams.params.index,
          Math.min(callParams.params.index + callParams.params.step, self.geneNames.length));

    postMessage({
        type: "expr vals",
        data: nextGeneNames,
      params: callParams.params
    })
  } else {
    postMessage({
      type: "complete",
      results: self.collectedResults,
      params: callParams.params
    })
  } // if.. else if(callParams.params.index < callParams.params.geneNames.length)
}

/**
 * Calculate differential expression between two groups of cells the Wilcoxon Mann-Whitney test
 * @param params A compound object containing data, and information passed to this worker from the event listener
 * @param geneData A sparse matrix containing the gene names being read in and the expression values
 */
function runWilcoxonOnGroup(params, geneData){

      for(var geneindex = 0; geneindex < geneData.array[0].length; geneindex++){

        //var selAexpr = [];
        //var selBexpr = [];

        var allValues = [];

        //retrieve expression data by indexes for selection A
        for(var cell = 0; cell < self.selAidx.length; cell++){
          allValues.push({
            selection: 1,
            exprVal: geneData.array[self.selAidx[cell]][geneindex]
          });
        }

        //retrieve expression data by indexes for selection B
        for(var cell = 0; cell < self.selBidx.length; cell++){
          allValues.push({
            selection: 2,
            exprVal: geneData.array[self.selBidx[cell]][geneindex]
          });
        }

        // Sort and calculate total ranks
        allValues.sort(function(x,y){return x.exprVal - y.exprVal});


        var lastVal = allValues[0].exprVal;
        var lastValStartIndex = 0;

        for (var i = 0; i < allValues.length; i++) {
          if (allValues[i] === lastVal) {
            // There is a tie

          } else {
            allValues[i].rank = i + 1;
          }


          allValues[i].rank = i +  1;
          /*
          if (allValues[i].selection == 1){
            totalRankA += (i+1);
          } else {
            totalRankB += (i+1);
          }
          */
        }

        // var totalRankA = 0;
        // var totalRankB = 0;

        // Sort out ties
        var startLastVal = 0;
        var lastVal = allValues[0].exprVal;
        for (var i = 0; i < allValues.length; i++) {

        }

        var lengthA = self.selAidx.length;
        var lengthB = self.selBidx.length;

        var u1 = totalRankA - (lengthA * (lengthA +1)) /2
        var u2 = totalRankB - (lengthB * (lengthB +1)) /2

        var U = Math.min(u1, u2);

        // Normal approximation
        var muU = lengthA * lengthB /2;
        var sigmaU = Math.sqrt(lengthA * lengthB * (lengthA + lengthB + 1) / 12)
        var z = (U - muU) / sigmaU;
        var zAbs = Math.abs(z);

        //accepts p < .05
        if(zAbs >= 3.0){
          self.collectedResults.push(
            {
              Z: z,
              absZ: zAbs,
              name: geneData.colnames[geneindex],
              fe: 0,
              M: 0,
              highest: false
            }
          )
        } // if(z >= 3.0)
      }
}
