"use strict";

function runKSonGroup(params, geneData){
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
            currD = (1-(selBexpr.length/pdfStepB));
          }
        }
        else if(selBexpr.length === 0){
          while(selAexpr.length > 0){
            fold += (curX-prevX) * currD;
            prevX = curX;
            curX = selAexpr.shift()
            currD = -(1-(selAexpr.length/pdfStepA));
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

function runWilcoxonOnGroup(params,geneData){
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

        var length = selAexpr.length;
        var lengthPrime = selBexpr.length;
        var index = 0;

        var totalArank = 0;
        var mean = 0;
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

        var totalBrank = length * lengthPrime - totalArank;



        var fold = Math.max(totalBrank,totalArank)/Math.min(totalBrank,totalArank) - 1;



        var mu = (length * lengthPrime) / 2;
        var sigma = (length * lengthPrime) * (length+ lengthPrime +1)/12;

        var z = Math.abs((Math.max(totalArank,totalBrank) - mu)/sigma);
        console.log(z);
        var zSign = (Math.max(totalBrank,totalArank) === totalArank ? 1 : -1);
        if(z >= 3.0){
          params.results.push({Z:(z*zSign), absZ:z, name: geneData.colnames[gene], fe: fold, M:mean, highest:(zSign >= 0)})
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
    if(callParams.params.method === "default" || callParams.params.method === "ksTest"){
      runKSonGroup(callParams.params, callParams.command.data);
    }
    else if(callParams.params.method === "wilcoxon"){
      runWilcoxonOnGroup(callParams.params, callParams.command.data);
    }
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






