"use strict";

/*
 * Filename: lightDeWorker.js
 * Author: Nikolas Barkas
 * Date: December 2017
 * Description: a light-weight worker thread for differential expression
 */

// Main event listener for the worker thread
self.addEventListener("message", function(e) {
    var messageData = e.data;

    if (messageData.type === "rundiffexpr") {
        // Just process the data and signal the result when ready

        // This is inefficient
        var selAidx = [];
        var selBidx = [];
        for (var i = 0; i < messageData.data.DimNames2.length; i++) {
            var idx = messageData.selections[0].indexOf(messageData.data.DimNames2[i]);
            if (idx !== -1) {
                selAidx.push(i);
            } else {
                selBidx.push(i);
            }
        }

        // Perform differential expression
        //var t0 = performance.now();
        var exprMatrix = getFullMatrix(messageData.data);
        var deres = runMannWhitneyIteration(exprMatrix, selAidx, selBidx);
        //var t1 = performance.now();
        //console.log("Diff expression took " + (t1 - t0) + "ms.");

        postMessage({
            type: "complete",
            results: deres,
            params: null
        });
    } else {
        console.error('Unknown action')
    }
}, false);


/**
 * Calculate differential expression between two groups of cells the Wilcoxon Mann-Whitney test
 * @param geneData A sparse matrix containing the gene names being read in and the expression values
 */
function runMannWhitneyIteration(geneData, selAidx, selBidx) {
    var collectedResults = [];

    const zcutoff = 3.0;
    const log2 = Math.log(2);
    //var geneCount = geneData.array[0].length;
    var geneCount = geneData.array.length;

    for (var geneindex = 0; geneindex < geneCount; geneindex++) {

        // Notify the parent thread of progress every n iterations
        if (geneindex % 100 === 0) {
            postMessage({
                type: "progressupdate",
                current: geneindex,
                outoff: geneCount
            });
        }

        var allValues = [];

        var nNonZeroA = 0;
        var nNonZeroB = 0;

        //retrieve expression data by indexes for selection A
        var selAlength = selAidx.length;
        for (var cell = 0; cell < selAlength; cell++) {
            //var eVal = geneData.array[selAidx[cell]][geneindex];
            var eVal = geneData.array[geneindex][selAidx[cell]];
            if (eVal != 0) nNonZeroA++;
            allValues.push({
                selection: 1,
                exprVal: eVal
            });
        }

        //retrieve expression data by indexes for selection B
        var selBlength = selBidx.length;
        for (var cell = 0; cell < selBlength; cell++) {
            //var eVal = geneData.array[selBidx[cell]][geneindex];
            var eVal = geneData.array[geneindex][selBidx[cell]];
            if (eVal != 0) nNonZeroB++;
            allValues.push({
                selection: 2,
                exprVal: eVal
            });
        }

        // Sort and calculate total ranks
        allValues.sort(function(x, y) {
            return x.exprVal - y.exprVal
        });

        // For keeping track of ties
        var lastVal = allValues[0].exprVal;
        var lastValStartIndex = 0;
        var inTie = false;

        // Calculate element ranks taking into account ties
        var allValueslength = allValues.length
        var i = -1;
        while (1) {
            i++;
            if (i === allValueslength) {
                if (inTie) {
                    // We have effectively just exited a tie
                    lastValStartIndex = Math.max(0, lastValStartIndex);
                    var commonRank = (lastValStartIndex + i + 1) / 2;
                    for (var j = lastValStartIndex; j < i; j++) {
                        allValues[j].rank = commonRank;
                    } // for
                }
                break;
            }

            // Set the rank to the position in the array (1-indexed)
            allValues[i].rank = i + 1;

            if (allValues[i].exprVal === lastVal) {
                // In a tie
                if (!inTie) {
                    // Just entered the tie
                    lastValStartIndex = i - 1;
                    lastVal = allValues[i].exprVal;
                    inTie = true;
                }
                // else we were already in a tie and we don't need to do anything
            } else {
                // Not in a tie
                if (inTie) {
                    // Just exited the previous tie
                    lastValStartIndex = Math.max(0, lastValStartIndex);
                    var commonRank = (lastValStartIndex + i + 1) / 2;
                    for (var j = lastValStartIndex; j < i; j++) {
                        allValues[j].rank = commonRank;
                    } // for
                    inTie = false;
                }
                lastVal = allValues[i].exprVal;
                lastValStartIndex = i;
            } // if == lastVa else
        } // for i

        // Calculate rank sum for A
        var totalRankA = 0;
        for (var i = 0; i < allValueslength; i++) {
            if (allValues[i].selection === 1) {
                totalRankA += allValues[i].rank;
            }
        }

        // Calculate U values
        var lenghtAxlengthB = selAlength * selBlength; //u1u2
        var u1 = totalRankA - (selAlength * (selAlength + 1)) / 2;
        var u2 = lenghtAxlengthB - u1;
        var U = Math.min(u1, u2);

        // Calculate rank abundance and Loop over rank counts and calculate K
        var rankCounts = [];
        for (var i = 0; i < allValueslength; i++) {
            var rank = allValues[i].rank;
            if (typeof rankCounts[rank] === 'undefined') {
                rankCounts[rank] = 1;
            } else {
                rankCounts[rank] = rankCounts[rank] + 1;
            }
        }
        var K = 0;
        var n = selAlength + selBlength;
        var ranks = Object.keys(rankCounts);
        var ranksLength = ranks.length;
        for (var i = 0; i < ranksLength; i++) {
            var ti = rankCounts[ranks[i]];
            K = K + ((Math.pow(ti, 3) - ti) / (n * (n - 1)));
        }

        // Calculate corrected sigma
        var sigmaUcorr = Math.sqrt((lenghtAxlengthB / 12) * (n + 1) - K);

        // Perform Normal approximation with tie correction
        // Calculate corrected Z score
        //var muU = lenghtAxlengthB / 2;        
        //var z = (U - muU) / sigmaUcorr;
        var z = (U - (lenghtAxlengthB / 2)) / sigmaUcorr;

        var zAbs = Math.abs(z);
        if (zAbs >= zcutoff) {
            var sumA = 0;
            for (var cell = 0; cell < selAlength; cell++) {
                sumA += geneData.array[geneindex][selAidx[cell]];
            }
            var meanA = sumA / selAlength + 1e-16;

            var sumB = 0;
            for (var cell = 0; cell < selBlength; cell++) {
                sumB += geneData.array[geneindex][selBidx[cell]];
            }
            var meanB = sumB / selBlength + 1e-16;

            var M = Math.log(meanA / meanB) / log2;
            if (M > 0) {
                z = zAbs;
            } else {
                z = -zAbs;
            }

            collectedResults.push({
                Z: z,
                absZ: zAbs,
                name: geneData.rownames[geneindex],
                fe: 0,
                M: M,
                highest: false
            })

        } // zAbs >= zcutoff

    }

    return collectedResults;
}



function getFullMatrix(data) {

    // Make a zero filled array (transposed)
    var out = Array(data.Dim[0]);
    for (var k = 0; k < data.Dim[0]; k++) {
        var row = Array(data.Dim[1]);
        for (var j = 0; j < data.Dim[1]; j++) {
            row[j] = 0;
        }
        out[k] = row;
    }

    // index in p (the column number)
    for (var j = 0; j < data.p.length - 1; j++) {
        // row start index, row end index (in x and i)
        var rsi = data.p[j];
        var rei = data.p[j + 1] - 1;

        // k is an index in x and i with the current column
        for (var k = rsi; k < rei; k++) {
            // row number
            var rn = data.i[k];

            // We want the transpose
            out[rn][j] = data.x[k];
        }
    }

    var retVal = {
        array: out,
        rownames: data.DimNames1,
        colnames: data.DimNames2
    };

    return retVal;
}