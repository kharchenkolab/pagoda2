"use strict";

// Main event listener for the worker thread
self.addEventListener("message", function(e){
var callParams = e.data;

  if(callParams.command.type === "setup"){
    handleSetupCommand(e);
  } else if(callParams.command.type === "initiate"){
    handleInitiateCommand(e);
  } else if(callParams.command.type === "process"){
    handleProcessCommand(e);
  } else if(callParams.command.type === "stop"){
    handleStopCommand(e);
  }
},false);

// Handlers for the various commands
function handleSetupCommand(e) {
     /*
    * Setup command
    * Worker requests cell data from its master
    * request type: Cell order
    */
    var callParams = e.data;
      var response = {
      request:{
        type: "cell order"
      },
      params: callParams.params
    }
    postMessage(response);
}

function handleInitiateCommand(e) {
  var callParams = e.data;
    /*
   * Initiate command
   * Recieves cell name data from the master thread and initializes the selection index arrays
   * Requests a subset of the genes at a time
   * params step set to number of genes requested per request made
   * params index set to 0, index will represent next gene to interpret
   * params numCells is the number of cells
   * request expr vals
   * request data: contains the gene names that are being asked for
   */
      callParams.params.step = Math.max(Math.floor(callParams.params.geneNames.length/200),10);
    callParams.params.index = 0;
    callParams.params.numCells = callParams.command.data.length;

    //if there is only one selection given make the second selection off of the indexes of the cellOrderData keys
    if(callParams.command.selections.length === 1){
      callParams.params.selAidx = [];
      callParams.params.selBidx = [...callParams.command.data.keys()];
      //creates array of cell indexes based on their corresponding index in cell order array
      for(var i = 0; i < callParams.command.selections[0].length; i++){
        var idx = callParams.command.data.indexOf(callParams.command.selections[0][i]);
        if(idx !== -1){
          callParams.params.selAidx.push(idx);
        }
      }

    }
    //makes selection index arrays for two selections
    else if(callParams.command.selections.length === 2){
      callParams.params.selAidx = [];
      callParams.params.selBidx = [];
      for(var i = 0; i < callParams.command.selections[0].length; i++){
        var idx = callParams.command.data.indexOf(callParams.command.selections[0][i]);
        if(idx !== -1){
          callParams.params.selAidx.push(idx);
        }
      }
      for(var i = 0; i < callParams.command.selections[1].length; i++){
        var idx = callParams.command.data.indexOf(callParams.command.selections[1][i]);
        if(idx !== -1){
          callParams.params.selBidx.push(idx);
        }
      }
    }
    postMessage({
      request:{
        type: "expr vals",
        data: callParams.params.geneNames.slice(callParams.params.index,
          Math.min(callParams.params.index + callParams.params.step,callParams.params.geneNames.length)),
      },
      params: callParams.params
    });
}

function handleProcessCommand(e) {
  var callParams = e.data;
    /*
     * Process Command
     *
     * Process a chunk of data based on the chosen method then requests more data
     in a similar way to other unless no more data remains, otherwise requests death
     *
     * request 1
     * request type: expr vals
     * request data: contains the gene names that are being asked for next
     *
     * request 2
     * request type: clean death
     */

    if(callParams.params.method === "default" || callParams.params.method === "ksTest"){
      runKSonGroup(callParams.params, callParams.command.data);
    }
    else if(callParams.params.method === "wilcoxon"){
      runWilcoxonOnGroup(callParams.params, callParams.command.data);
    } else {
      // TODO: Handle error
    }

    //advance index to current spot
    callParams.params.index += callParams.params.step;

    //continue requesting data if data still needs to be read
    if(callParams.params.index < callParams.params.geneNames.length){
      postMessage({
        request:{
          type: "expr vals",
          data: callParams.params.geneNames.slice(callParams.params.index,
            Math.min(callParams.params.index + callParams.params.step, callParams.params.geneNames.length)),
        },
        params: callParams.params
      })
    } else{
      postMessage({
        request:{
          type: "clean death"
        },
        params: callParams.params
      })
    } // if.. else if(callParams.params.index < callParams.params.geneNames.length)
}

function handleStopCommand(e) {
  var callParams = e.data;
    /*
   * User has issued a stop command
   * Halts progress of analysis and sends the main thread a message to kill this worker and not call the callback
   */
      postMessage({
        request:{
          type: "abrupt death"
        },
      })
}


/**
 * Calculate differential expression between two groups of cells the Wilcoxon Mann-Whitney test
 * @param params A compound object containing data, and information passed to this worker from the event listener
 * @param geneData A sparse matrix containing the gene names being read in and the expression values
 */
function runWilcoxonOnGroup(params,geneData){

      //for each gene calculate differential expression
      for(var gene = 0; gene < geneData.array[0].length; gene++){

        var selAexpr = [];
        var selBexpr = [];

        //retrieve expression data by indexes for selection A
        for(var cell = 0; cell < params.selAidx.length; cell++){
          selAexpr.push(geneData.array[params.selAidx[cell]][gene]);
        }
        //retrieve expression data by indexes for selection B
        for(var cell = 0; cell < params.selBidx.length; cell++){
          selBexpr.push(geneData.array[params.selBidx[cell]][gene]);
        }

        selAexpr.sort(function(x,y){return x-y});
        selBexpr.sort(function(x,y){return x-y});

        //removes all selections with a value of 0
        while(selAexpr.length >0 && selAexpr[0] === 0){
          selAexpr.shift();
        }
        while(selBexpr.length >0 && selBexpr[0] === 0){
          selBexpr.shift();
        }

        //skips the gene if there aren't enough valid cells
        if(selAexpr.length < 10 || selBexpr.length < 10){
          continue;
        }

        var length = selAexpr.length;
        var lengthPrime = selBexpr.length;
        var index = 0;

        var totalArank = 0;
        var mean = 0;

        //calculates ranks for cell selection A by way of wilcoxon ranking
        for(var i = 0; i< selBexpr.length; i++){
          while(index < selAexpr.length && selAexpr[index] <= selBexpr[i]){
            if(selAexpr[index] === selBexpr[i]){
              totalArank += (i + .5)
            }
            else{
              totalArank += (i);
            }
            mean += selAexpr[index];
            index++;
          }
          mean += selBexpr[i];
        }
        for(; index < selAexpr.length; index++){
          totalArank += (selBexpr.length);
          mean += selAexpr[index];
        }
        mean = mean/(length + lengthPrime);

        //calculates total of B's ranks using A's rank and sample size
        var totalBrank = length * lengthPrime - totalArank;

        var fold = (totalBrank === 0? 1: Math.min(totalArank/totalBrank,1));

        var mu = (length * lengthPrime) / 2;
        var sigma = Math.sqrt((length * lengthPrime) * (length+ lengthPrime +1)/12);

        var z = Math.abs((Math.max(totalArank,totalBrank) - mu)/sigma);
        var zSign = (Math.max(totalBrank,totalArank) === totalArank ? 1 : -1);

        //accepts p < .05
        if(z >= 3.0){
          params.results.push(
            {
              Z:(z*zSign),
              absZ:z,
              name: geneData.colnames[gene],
              fe: fold,
              M:mean, highest:(zSign >= 0)
            }
          )
        } // if(z >= 3.0)
      }
}
