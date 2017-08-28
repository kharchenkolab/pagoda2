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

        // For keeping track of ties
        var lastVal = allValues[0].exprVal;
        var lastValStartIndex = 0;
        var inTie = false;
        
        // Calculate element ranks taking into account ties
        for (var i = 0; i < allValues.length; i++) {
          // Set the rank to the position in the array (1-indexed)
          allValues[i].rank = i + 1;
          
          if (allValues[i].exprVal === lastVal) {
            // In a tie
            if (!inTie) {
              // Just entered the tie
              lastValStartIndex = i - 1;
              inTie = true;
            } 
            // else we were already in a tie and we don't need to do anything
          } else {
            // Not in a tie
            if (inTie) {
              // Just exited the previous tie
              var commonRank = (lastValStartIndex + 1) + (i + 1) / (i - lastValStartIndex);
              for (var j = lastValStartIndex; j < i; j++) {
                allValues[j].rank = commonRank;
              } // for
              
              inTie = false;
            } else { // Not in tie
              lastVal = allValues[i].exprVal;
              lastValStartIndex = i;
            } // if inTie
          } // if == lastVa else
        } // for i

        // Calculate rank sums
        var totalRankA = 0;
        var totalRankB = 0;
        for (var i = 0; i < allValues.length; i++) {
          if (allValues[i].selection == 1){
            totalRankA += (i+1);
          } else {
            totalRankB += (i+1);
          }
        }

        var lengthA = self.selAidx.length;
        var lengthB = self.selBidx.length;

        var u1 = totalRankA - (lengthA * (lengthA +1)) /2
        var u2 = totalRankB - (lengthB * (lengthB +1)) /2

        var U = Math.min(u1, u2);

        // Normal approximation
        var muU = lengthA * lengthB /2;
        var sigmaU = Math.sqrt(lengthA * lengthB * (lengthA + lengthB + 1) / 12)
        
        // Calculate rank abundance 
        var rankCounts = {};
        for (var i = 0; i < allValues.length; i++) {
          var rank = allValues[i].rank;
          if(typeof rankCounts[rank] === 'undefined') {
            rankCounts[rank]= 1;
          } else {
            rankCounts[rank] = rankCounts[rank] + 1;
          }
        }
        
        // TODO: Loop over rankCounts and calculate K value
        
        
        
        
        // var sigmaUcorr = Math.sqrt(lengthA * lengthB / 12 ( lengthA + lengthB + 1 ) - K)
        
        var z = (U - muU) / sigmaU;
        // var z = (U - muU) / sigmaUcorr;
        
        var zAbs = Math.abs(z);

        // accepts p < .05
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
