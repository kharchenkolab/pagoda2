"use strict";

function step(params, geneData){
      for(var gene = 0; gene < geneData.array[0].length; gene++){

        var selAexpr = [];
        var selBexpr = [];

        for(var cell = 0; cell < params.selAidx.length; cell++){
          selAexpr.push(geneData.array[params.selAidx[cell]][gene]);
        }
        for(var cell = 0; cell < params.selBidx.length; cell++){
          selBexpr.push(geneData.array[params.selBidx[cell]][gene]);
        }



        selAexpr.sort(function(x,y){return x-y});
        selBexpr.sort(function(x,y){return x-y});

        while(selAexpr.length >0 && selAexpr[0] === 0){
          selAexpr.shift();
        }
        while(selBexpr.length >0 && selBexpr[0] === 0){
          selBexpr.shift();
        }
        if(selAexpr.length < 10 || selBexpr.length < 10){
          continue;
        }

        var d = 0;
        var dSign = 0;
        var totalA = 0;
        var totalB = 0;
        var pdfStepA = selAexpr.length;
        var pdfStepB = selBexpr.length;
        var firstX = Math.min(selAexpr[0], selBexpr[0]);
        var prevX = firstX;
        var curX = prevX;
        var currD = 0;
        var fold = 0;

        while(selAexpr.length > 0 && selBexpr.length > 0){
          fold += (curX-prevX) * currD;
          prevX = curX;
          curX = Math.min(selAexpr[0], selBexpr[0]);
          if(selAexpr[0] < selBexpr[0]){
            totalA += selAexpr.shift();
          } else if(selAexpr[0] > selBexpr[0]){
            totalB += selBexpr.shift();
          } else{
            while(selAexpr[0] === selBexpr[0]){
              if(selAexpr.length > 0){
                totalA += selAexpr.shift();
              }
              if(selBexpr.length > 0){
                totalB += selBexpr.shift();
              }
              if(selBexpr.length + selAexpr.length === 0){
                break;
              }
            }
          }

          currD = (1-(selBexpr.length/pdfStepB)) - (1-(selAexpr.length/pdfStepA));
          var instDSign = Math.sign(currD);
          var instD = Math.abs(currD);
          if(instD > d){
            d = instD;
            dSign = instDSign;
          }

        }

        if(selAexpr.length === 0){
          while(selBexpr.length > 0){
            fold += (curX-prevX) * currD;
            prevX = curX;
            curX = selBexpr.shift()
            currD = 1 - (1-(selBexpr.length/pdfStepB));
          }
        }
        else if(selBexpr.length === 0){
          while(selAexpr.length > 0){
            fold += (curX-prevX) * currD;
            prevX = curX;
            curX = selAexpr.shift()
            currD = 1 - (1-(selAexpr.length/pdfStepA));
          }
        }
        fold += (curX-prevX) * currD;


        var mean = Math.log(totalA/pdfStepA)-Math.log(totalB/pdfStepB)
        fold = fold/(curX - firstX)
        var z = d * Math.sqrt(pdfStepA*pdfStepB/(pdfStepA+pdfStepB));
        if(!(totalA === 0 || totalB === 0) && z >= 3.0){
          params.results.push({Z:(z*dSign), absZ:z, name: geneData.colnames[gene], fe: fold, M:mean, highest:(dSign >= 0)})
        }
      }
}
self.addEventListener("message", function(e){
  var callParams = e.data;
  if(callParams.command.type === "setup"){
    var response = {
      request:{
        type: "cell order"
      },
      params: callParams.params
    }
    postMessage(response);
  }
  //initiates the workers statistic modules
  else if(callParams.command.type === "initiate"){
    callParams.params.step = Math.max(Math.floor(callParams.params.geneNames.length/200),10);
    callParams.params.index = 0;
    callParams.params.numCells = callParams.command.data.length;

    if(callParams.command.selections.length === 1){
      callParams.params.selAidx = [];
      callParams.params.selBidx = [...callParams.command.data.keys()];
      for(var i = 0; i < callParams.command.selections[0].length; i++){
        var idx = callParams.command.data.indexOf(callParams.command.selections[0][i]);
        if(idx !== -1){
          callParams.params.selAidx.push(idx);
        }
      }

    }
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
        data: callParams.params.geneNames.slice(callParams.params.index, Math.min(callParams.params.index + callParams.params.step,callParams.params.geneNames.length)),
      },
      params: callParams.params
    });
  }
  // tells statistic module to process a clump of data
  else if(callParams.command.type === "process"){

    step(callParams.params, callParams.command.data);
    callParams.params.index += callParams.params.step;//advance index to current spot
    if(callParams.params.index < callParams.params.geneNames.length){
      postMessage({
        request:{
          type: "expr vals",
          data: callParams.params.geneNames.slice(callParams.params.index, Math.min(callParams.params.index + callParams.params.step, callParams.params.geneNames.length)),
        },
        params: callParams.params
      })
    }
    else{
      postMessage({
        request:{
          type: "clean death"
        },
        params: callParams.params
      })
    }
  }

  else if(callParams.command.type === "stop"){
      postMessage({
        request:{
          type: "abrupt death"
        },
      })
  }
},false);

/*function runKSTestOnTwo(selectionA,selectionB){

  var worker = this;
  var dataCntrl = new dataController();
  dataCntrl.getGeneInformationStore(function(geneNameData){
    var geneNames = [];
    for(var i = 0; i < geneNameData.localData.length; i++){
      geneNames.push(geneNameData.localData[i].genename);
    }
    geneNameData = undefined;

    var stepWiseCall = async function(params, index, step, max){
      var runSet = params.geneNames.slice(index, index+step);
      var geneData = undefined;
      dataCntrl.getExpressionValuesSparseByCellIndex(runSet,0,params.cellDataLength,function(data){
          geneData = data;
      })

      while(geneData === undefined){
        await (new Promise(resolve => setTimeout(resolve, 1)));
      }
      for(var gene = 0; gene < step; gene++){

        var selAexpr = [];
        var selBexpr = [];

        for(var cell = 0; cell < params.selAidx.length; cell++){
          selAexpr.push(geneData.array[params.selAidx[cell]][gene]);
        }
        for(var cell = 0; cell < params.selBidx.length; cell++){
          selBexpr.push(geneData.array[params.selBidx[cell]][gene]);
        }



        selAexpr.sort(function(x,y){return x-y});
        selBexpr.sort(function(x,y){return x-y});

        var d = 0;
        var dSign = 0;
        var totalA = 0;
        var totalB = 0;
        var pdfStepA = selAexpr.length;
        var pdfStepB = selBexpr.length;
        var firstX = Math.min(selAexpr[0], selBexpr[0]);
        var prevX = firstX;
        var curX = prevX;
        var currD = 0;
        var fold = 0;

        while(selAexpr.length > 0 && selBexpr.length > 0){
          fold += (curX-prevX) * currD;
          prevX = curX;
          curX = Math.min(selAexpr[0], selBexpr[0]);
          if(selAexpr[0] < selBexpr[0]){
            totalA += selAexpr.shift();
          } else if(selAexpr[0] > selBexpr[0]){
            totalB += selBexpr.shift();
          } else{
            while(selAexpr[0] === selBexpr[0]){
              if(selAexpr.length > 0){
                totalA += selAexpr.shift();
              }
              if(selBexpr.length > 0){
                totalB += selBexpr.shift();
              }
              if(selBexpr.length + selAexpr.length === 0){
                break;
              }
            }
          }

          currD = (1-(selBexpr.length/pdfStepB)) - (1-(selAexpr.length/pdfStepA));
          var instDSign = Math.sign(currD);
          var instD = Math.abs(currD);
          if(instD > d){
            d = instD;
            dSign = instDSign;
          }

        }

        if(selAexpr.length === 0){
          while(selBexpr.length > 0){
            fold += (curX-prevX) * currD;
            prevX = curX;
            curX = selBexpr.shift()
            currD = 1 - (1-(selBexpr.length/pdfStepB));
          }
        }
        else if(selBexpr.length === 0){
          while(selAexpr.length > 0){
            fold += (curX-prevX) * currD;
            prevX = curX;
            curX = selAexpr.shift()
            currD = 1 - (1-(selAexpr.length/pdfStepA));
          }
        }
        fold += (curX-prevX) * currD;


        var mean = Math.log(totalA/pdfStepA)-Math.log(totalB/pdfStepB)
        fold = fold/(curX - firstX)
        var z = d * Math.sqrt(pdfStepA*pdfStepB/(pdfStepA+pdfStepB));
        if(!(totalA === 0 || totalB === 0) && z >= 3.0){
          params.results.push({Z:(z*dSign), absZ:z, name: geneData.colnames[gene], fe: fold, M:mean, highest:(dSign >= 0)})
        }
      }
    }

    var callback = function(params){
      var actionUI = new actionPanelUIcontroller();
      actionUI.currentDErequest = null;
      // Get the cell names in the selection for storing
      var cellSelCntr = new cellSelectionController();
      var selAcells = cellSelCntr.getSelection(selectionA);
      var selBcells = cellSelCntr.getSelection(selectionB);

      // Make a deResult set for saving the results
      // and save metadata related to this de result



      var end = new Date();
      var resultSet = new deResultSet();
      resultSet.setResults(params.results);
      resultSet.setName(params.resultName);
      resultSet.setSelectionA(selAcells);
      resultSet.setSelectionB(selBcells);
      resultSet.setStartTime(params.start);
      resultSet.setEndTime(end);
      // Save this de result set in the differentialExpresionStore
      var diffExprStore = new differentialExpressionStore();
      var setId = diffExprStore.addResultSet(resultSet);

      // Notify the DE results table to updata from the store
      var diffExpreTblView = new diffExprTableViewer();
      diffExpreTblView.update();


      diffExpreTblView.showSelectedSet(setId);
      worker.postMessage("I'm Done");
    }

    var params = {start: new Date(), results: [], selBidx: [], selAidx: [], geneNames: geneNames, resultName: "test"};

    dataCntrl.getCellOrder(function(cellData){
      var cellSelCntrl = new cellSelectionController();
      var selAcells = cellSelCntrl.getSelection(selectionA);
      var selBcells = cellSelCntrl.getSelection(selectionB);

      for(var i = 0; i < selAcells.length; i++){
        var index = cellData.indexOf(selAcells[i])
        if(index !== -1){
          params.selAidx.push(index);
        }
      }
      for(var i = 0; i< selBcells.length; i++){
        var index = cellData.indexOf(selBcells[i])
        if(index !== -1){
          params.selBidx.push(index);
        }
      }
      params.cellDataLength = cellData.length;

      var cellSelCtrl = new cellSelectionController();
      pagHelpers.generateProgressBar(stepWiseCall, geneNames.length, Math.max(Math.floor(geneNames.length/500),10), callback, params, "Local KS test: " + cellSelCtrl.getSelectionDisplayName(getselectionA) + " vs " + cellSelCtrl.getSelectionDisplayName(selectionB), false)
    })

  })
}*/





