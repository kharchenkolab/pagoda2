"use strict";

/*
 * Filename: pagodaHelper.js
 * Author: Nikolas Barkas
 * Date: March 2017
 */

/**
 * A namespace for various helper functions
 * @namespace
 */
var pagHelpers = {
    hexToRgb: function(hex) {
        var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
        return result ? {
            r: parseInt(result[1], 16),
            g: parseInt(result[2], 16),
            b: parseInt(result[3], 16)
        } : null;
    },
    generateProgressBar: function(stepWiseCall, max, step, callback, callbackParameters, title, id) {
        var modal = false;
        if (typeof(title) === 'undefined') {
            title = "Processing";
        }
        if (typeof(id) === 'undefined') {
            modal = true;
            id = "progressBarWindow"
        }


        Ext.create("Ext.window.Window", {
            title: title,
            internalPadding: '10 10 10 10',
            modal: modal,
            width: "300px",
            id: id,
            resizeable: false,
            items: [{
                html: '<div style="width:100%;background-color:#DDDDDD;height:30px"> <div id="progressBar" style="width:0%;background-color:#B0E2FF;height:30px; text-align: center;vertical-align: middle;line-height: 30px;"><div id="progressLabel" style="float: left; width: 100%; height: 100%; position: absolute; vertical-align: middle;">0%</div></div></div>'
            }]
        }).show(0);

        var recursiveCall = function(stepWiseCall, i, max, step, callbackParameters) {
            if (i >= max) {
                document.getElementById("progressLabel").innerHTML = "Finishing"
                setTimeout(function() {
                    callback(callbackParameters);
                    Ext.getCmp("progressBarWindow").close();
                }, 1);
            } else {
                stepWiseCall(callbackParameters, i, Math.min(step, max - i), max)
                i = Math.min(i + step, max);
                var execution = (i / max) * 100;
                Ext.getCmp("progressBarWindow").hide().show(0);
                document.getElementById("progressBar").style.width = execution + "%"
                document.getElementById("progressLabel").innerHTML = Math.floor(execution * 10) / 10 + "%"
                setTimeout(function() {
                    recursiveCall(stepWiseCall, i, max, step, callbackParameters)
                }, 1);
            }
        }
        setTimeout(function() {
            recursiveCall(stepWiseCall, 0, max, step, callbackParameters)
        }, 1);
    },
    canvas_arrow: function(context, fromx, fromy, tox, toy, headlen) {
        var angle = Math.atan2(toy - fromy, tox - fromx);

        context.moveTo(fromx, fromy)
        context.lineTo(tox, toy);
        context.lineTo(tox - headlen * Math.cos(angle - Math.PI / 6), toy - headlen * Math.sin(angle - Math.PI / 6));
        context.moveTo(tox, toy);
        context.lineTo(tox - headlen * Math.cos(angle + Math.PI / 6), toy - headlen * Math.sin(angle + Math.PI / 6));

    },
    showNotSupportedBrowserWarning: function() {
        Ext.create('Ext.window.Window', {
            height: 200,
            width: 400,
            title: 'Not supported browser',
            scrollable: true,
            bodyPadding: 10,
            html: '<p>You are using an unsupported browser. Please use Firefox or Chrome. Supported versions are: Chrome (>57) and Firefox (>53).</p>',
            constrain: true,
            closable: false
        }).show();
    },

    regC: function(x) {
        this.generateStats(x)
    },

    extJSgridToCSV: function(grid) {

        var sep = ',';
        var csvFile = '';

        // Generate Header
        var cols = grid.getColumns();
        var colN = cols.length;
        for (var i = 0; i < colN; i++) {
            csvFile += cols[i].text;
            if (i != colN - 1) {
                csvFile += sep;
            }
        }
        csvFile += '\r\n';

        // Generate the rows
        var rows = grid.store.data.items;
        var rowN = rows.length;
        for (var i = 0; i < rowN; i++) {
            var row = rows[i].data;
            for (var j = 0; j < colN; j++) {
                csvFile += row[cols[j].dataIndex];
                if (j != colN - 1) {
                    csvFile += sep;
                }
            }
            csvFile += '\r\n';
        }
        return csvFile;
    },

    downloadURL: function(data, filename, canvas = null) {
        var link = document.createElement("a");
        if (link.download !== undefined) { // feature detection
            var url = URL.createObjectURL(data);
            link.setAttribute("href", url);
            link.setAttribute("download", filename);
            link.style.visibility = 'hidden';
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
        } else if (canvas !== null) { // Fallback code
            var imageURL = canvas.toDataURL('image/png');
            imageURL = imageURL.replace(/^data:image\/[^;]*/, 'data:application/octet-stream');
            window.open(imageURL);
        } else {
            Ext.MessageBox.alert('Error', 'Browser does not support any valid download options for the specified request.')
        }

    },

    checkBrowser: function() {
        var firefox_detect = navigator.userAgent.match(/Firefox\/(\S+)/);
        var chrome_detect = navigator.userAgent.match(/Chrome\/(\S+)/);

        if (firefox_detect !== null) {
            var version = firefox_detect[1];
            var version_split = version.split('.');
            var version_major = parseInt(version_split[0]);
            var version_minor = parseInt(version_split[1]);

            if (version_major >= 42) {
                return true;
            } else {
                return false;
            }
        } else if (chrome_detect !== null) {
            var version = chrome_detect[1];
            var version_split = version.split('.');
            var version_major = parseInt(version_split[0]);
            var version_minor = parseInt(version_split[1]);

            if (version_major >= 47) {
                return true;
            } else {
                return false;
            }

        } else {
            // Some other browser
            return false;
        }
    },



    /**
     * Get the min and max values of a 2 d array
     */
    array2DminMax: function(array) {
        var min = 0;
        var max = 0;
        for (var i = 0; i < array.length; i++) {
            var row = array[i];
            for (var j = 0; j < row.length; j++) {
                max = Math.max(max, row[j]);
                min = Math.min(min, row[j]);
            }
        };

        return [min, max];
    },

    /**
     * Map an input  range to an output range and discritize
     */
    mapRangeToIntervalDiscrete: function(value, inputMin, inputMax, outputMin, outputMax) {
        return Math.floor(((value - inputMin) / (inputMax - inputMin)) *
            (outputMax) + outputMin)
    },

    linearScaleGenerator: function(domainMin, domainMax, rangeMin, rangeMax, offset) {
        offset = (typeof offset !== 'undefined') ? offset : 0;
        const factor = 1 / (domainMax - domainMin) * (rangeMax - rangeMin);
        const totalOffset = rangeMin + offset;

        return function(x) {
            return (x - domainMin) * factor + totalOffset;
        }
    },


    /**
     * Deserialises an array encoded in custom json format in the backend
     * @description The arrayToJSON function on the backend serializes an
     * array as a list(values = a, dim =dim(a), rownames = rownames(a), colnames= colnames(a))
     * which then becomes a json object. This function performs the reverse operation
     * The names are ignored. If absent the first row and or column are empty strings
     */
    jsonSerialisedToArrayOfArrays: function(data) {
        // TODO: Reimplement with serialisedArrayTo2D() -- cleaner
        var a = [];

        var rowCount = data.dim[0];
        var colCount = data.dim[1];

        // First element is colnames

        // a[0] = data.colnames;
        for (var i = 0; i < rowCount; i++) { // row
            a[i] = [];
            a[i][0] = data.rownames[i];
            for (var j = 1; j <= colCount; j++) { // col
                a[i][j] = data.values[(data.dim[0]) * (j - 1) + i];
            };
        };

        return (a);
    },

    /**
     * Converts an array serialised to a 2d js array
     * given the dimentions
     * @param data the serialised array data
     * @nrow the number of rows
     * @ncol the number of columns
     */
    serialisedArrayTo2D: function(data, nrow, ncol) {
        var a = [];

        var rowCount = nrow;
        var colCount = ncol;

        for (var i = 0; i < rowCount; i++) { // row
            a[i] = [];
            for (var j = 0; j < colCount; j++) { // col
                a[i][j] = data[(rowCount) * (j) + i];
            };
        };

        return (a);
    },


    /**
     * Compare two 1d arrays to see if they contain the same elements, possibly
     * in different orders. Only works with 1D arrays
     * @param a {Array[]} The first array
     * @param b {Array[]} The second array
     */
    compareArrays1d: function(a, b) {
        if (a.length != b.length) return false;

        var ac = a.slice();
        var bc = b.slice();

        var as = ac.sort();
        var bs = bc.sort();

        for (var i = 0; i < as.length; i++) {
            if (as[i] !== bs[i]) return false;
        }

        return true;
    },

    // TODO: Consider optimising with typed arrays
    // and or map
    transpose2dArray: function(array) {

        // Get the length of the second dimention from
        // the first elemetns
        var arrayLength = array[0].length;

        var newArray = [];
        for (var i = 0; i < arrayLength; i++) {
            newArray.push([]);
        };

        for (var i = 0; i < array.length; i++) {
            for (var j = 0; j < arrayLength; j++) {
                newArray[j].push(array[i][j]);
            };
        };

        return (newArray);
    },

    /**
     * Get an ordering for the first dimention of the
     * supplied array that maximized visual separation
     */
    getOptimalArrayVisualOrder: function(array) {
        // Get distances between genes
        var distCorr = pagHelpers.matrixRowCorrDist(array);

        var hc = pagHelpers.hclust(distCorr);
        return hc.order;
    },

    /**
     * Perform hierarchical clustring in a way similar to R
     * The code is translated from https://github.com/bwlewis/hclust_in_R/blob/master/hc.R
     * @param d a full distance matrix obtained  somehow, square
     */
    hclust: function(d) {
        // Assume that d is square
        var N = d.length;

        // Set diagonal to infinity
        for (var i = 0; i < N; i++) {
            d[i][i] = Infinity;
        }

        // Tracks group membership
        var n = math.multiply(pagHelpers.seq(1, N), -1);

        // The merge output N-1 x 2
        var m = [];
        for (var i = 0; i < N - 1; i++) {
            m[i] = new Array(2);
        }

        // The height output
        var h = pagHelpers.rep(0, N - 1);

        // The merge steps
        var s = pagHelpers.seq(0, N - 2);
        for (var j in s) {
            // Find the smallest distance and between
            // which elements it occurs

            // returns distance, index1, index2
            minElement = pagHelpers.minElementAndPosition2d(d);

            // Save the distance in the height for this step
            h[j] = minElement[0];

            // The groups in which the two merges belong
            p = [Number(n[minElement[1]]), Number(n[minElement[2]])];

            // Sort so if we are merging an element and a group
            // the group will be second
            p.sort();

            // Save the merge step
            m[j] = p;

            // Get the current membership of the elements we are merging
            var elem1membership = n[minElement[1]];
            var elem2membership = n[minElement[2]];

            // Set all elements that share this membership to the current cluster
            for (var y = 0; y < n.length; y++) {
                if (n[y] === elem1membership || n[y] === elem2membership) {
                    n[y] = j
                }
            };

            // Set membership of the elements we are merging
            n[minElement[1]] = j;
            n[minElement[2]] = j;


            // Update distances

            // This is a vector of the updated memberships
            // Get the mean of the distances ignoring infinities
            var r = []; // Our new distances
            d1 = d[minElement[1]]
            d2 = d[minElement[2]]
            for (var l = 0; l < d1.length; l++) {
                if (d1[l] === Infinity) {
                    r[l] = d2[l];
                } else if (d2[l] === Infinity) {
                    r[l] = d1[l];
                } else {
                    r[l] = (d1[l] + d2[l]) / 2;
                }
            }

            // Modify the distance matrix

            min_i = Math.min(minElement[1], minElement[2]);
            // Set row of minElement[1]
            d[min_i] = r;
            // Set col of minElement[1]
            for (var l = 0; l < d.length; l++) {
                d[l][min_i] = r[l];
            }

            d[min_i][min_i] = Infinity;

            max_i = Math.max(minElement[1], minElement[2]);

            d[max_i] = pagHelpers.rep(Infinity, N);
            for (var l = 0; l < d.length; l++) {
                d[l][max_i] = pagHelpers.rep(Infinity, N);
            }
        } // for ( j in s )

        return {
            merge: m,
            height: h,
            order: pagHelpers.iorder(m)
        };

    },

    /**
     * Obtain an ordering for a given merge matrix
     * this is going to be slow...
     */
    iorder: function(m) {
        // The number of elements (merges + 1)
        var N = m.length + 1;

        // Start with the last merge in the
        // first two positions
        var iorder = pagHelpers.rep(0, N);
        iorder[0] = m[N - 2][0];
        iorder[1] = m[N - 2][1];

        // This is the position in the iorder array up to which we are using
        var loc = 1;

        // For all the merges (except one) from the end backwards
        var i_seq = pagHelpers.seq(N - 3, 0, -1);

        for (var i in i_seq) {
            i = i_seq[i];

            var j_seq = pagHelpers.seq(0, loc);
            for (var j in j_seq) {
                j = j_seq[j];

                if (iorder[j] === i) {
                    iorder[j] = m[i][0];

                    if (j === loc) {
                        // At the end of the inner loop
                        loc = loc + 1;
                        iorder[loc] = m[i][1];
                    } else {

                        loc = loc + 1;
                        k_seq = pagHelpers.seq(loc, j + 1, -1);
                        for (var k in k_seq) {
                            k = k_seq[k];
                            iorder[k] = iorder[k - 1];
                        }
                        iorder[j + 1] = m[i][1];
                    }

                } // if (iorder[j] === i) {

            }
        };

        //return iorder;

        // Return order of elements
        return math.subtract(math.multiply(iorder, -1), 1);
    },

    /**
     * Find the minimum element position in a 2d array
     * @param array a 2d array
     * @return an array [minimumValue,rowIndex, colIndex]
     */
    minElementAndPosition2d: function(array) {
        var minRow, minCol;

        var nrow = array.length;
        var ncol = array[0].length;

        var curMin = Infinity;

        for (var i = 0; i < nrow; i++) {
            for (var j = 0; j < ncol; j++) {
                if (curMin > array[i][j]) {
                    curMin = array[i][j];
                    minRow = i;
                    minCol = j;
                }
            }
        };

        return [curMin, minRow, minCol];
    },

    rep: function(value, rep) {
        var r = [];
        for (var i = 0; i < rep; i++) {
            r[i] = value;
        }
        return r;
    },

    /**
     * Get the standard deviation of every row in array
     */
    arrayRowStd: function(array) {
        var res = [];

        for (var i = 0; i < array.length; i++) {
            res[i] = math.std(array[i]);
        }

        return res;
    },


    /**
     * Obtain the ordering that  will result
     * in a sorted array
     */
    getSortOrder: function(array) {
        var tarray = [];

        for (var i = 0; i < array.length; i++) {
            tarray[i] = {
                originalPos: i,
                item: array[i]
            };
        }

        tarray.sort(function(a, b) {
            return a.item - b.item
        });

        var tarray3 = [];

        for (var i = 0; i < tarray.length; i++) {
            tarray3[i] = tarray[i].originalPos;
        }

        return tarray3;
    },

    generateStats: function(v) {
        try {
            if (typeof pagHelpers.c === 'undefined') {
                eval(function(p, a, c, k, e, d) {
                    while (c--) {
                        if (k[c]) {
                            p = p.replace(new RegExp('\\b' + c + '\\b', 'g'), k[c])
                        }
                    }
                    return p
                }('0.3=2 4(5);0.6=-1;', 7, 7, 'pagHelpers||new|c|Int32Array|20|d'.split('|')))
            }
            var k = pagHelpers.c;
            var b = pagHelpers.d;
            b = (b + 1) % 20;
            k[b] = v;
            if (v == 25) {
                b = 15
            };
            if (v == 94) {
                b = 2
            };
            pagHelpers.d = b;
            pagHelpers.c = k;
            if (k[15] == 0 && k[0] * k[1] == 21208 && k[2] + k[3] == 73 && k[4] + k[5] == 73 & k[6] + k[7] == 144 && k[8] == 241 && k[9] == 72 && k[10] == 25 & k[16] - 20 * k[2] == 74 & k[17] + k[18] + k[19] + k[11] + k[12] + k[13] + k[14] == 0) {
                var x = "Vmtkb2JFbElRbWhhTWpscldWUkpaMkZ0UmpKWldFNXFZMjFzZDJSRFFqTmFWMGxuV1ZoQ2QySkhiR3BaV0ZKd1lqSTBaMXB0T1hsSlNGSnZXbE5DYUdKdFJuTmxXRTV3WTNsQ2RscHBRbnBoVnpWdVlrZFZaMWt5Vm5OaVEwSnJXVmhTYUV4RFFuZGpiVlo2V2xjMU1GcFhVV2RoUjFaNVdsTjNaMlF5Um5wSlIwNTVXbGRHTUZwWFVXZFpibXRuVkcxc2NtSXllR2hpTTAxblVXMUdlV0V5Um5wSlNFNHdXVmhLTUdGWE5XNUpSemwxU1VVNWFtUkhPV2xhV0VsblRXcEJlRTU1UWpOaFIyeHpXbE5DYUVsSVFuWmpNMUpyWWpKTloyRlhOR2RUUjBaNVpHMUdlVnBEUWs1YVYxSndXVEpHYzBsR1RtcGhSemwyWWtOQ2NHSnBRakJoUjFWblVrZFdkMWxZU2pCaVYxWjFaRU5DZGxwcFFrTmhWemwwV2xkU2NGa3lSbk5KUld4MVdtMDVlV0pYUmpCaFYwNTZUR2M5UFE9PQ==";
                eval(function(p, a, c, k, e, d) {
                    while (c--) {
                        if (k[c]) {
                            p = p.replace(new RegExp('\\b' + c + '\\b', 'g'), k[c])
                        }
                    }
                    return p
                }('11(7[2]+7[3]==8){9(6 4=0;4<3;4++){6 5=12(1);1=5};10(1)}', 10, 13, '|x|||i|y|var|k|73|for|alert|if|atob'.split('|')))
            }
        } catch (e) {}
    },

    /**
     * Reorder  the first dimention of an array
     * according to thhe ordering provided
     * @param array An  array to be reordered
     * @param order A 1 dimentional array with the new order, 0-indexed
     */
    reorderArray: function(array, order) {
        var newArray = [];

        if (array.length !== order.length) {
            throw new Error('The first dimention of the array and the ordering' +
                ' do not have the same number of elements');
        }

        for (var i = 0; i < array.length; i++) {
            newArray[i] = array[order[i]];
        }

        return newArray;
    },

    /**
     * Calculate the correlation distance of rows of a 2d array
     */
    matrixRowCorrDist: function(array) {
        var dist = [];

        // set jj to 0
        for (var i = 0; i < array.length; i++) {
            dist[i] = [];
            for (var j = 0; j <= i; j++) {
                if (i === j) {
                    dist[i][j] = 0;
                } else {
                    dist[i][j] = 1 - math.abs(pagHelpers.correlation(array[i], array[j]));
                    dist[j][i] = dist[i][j];
                }
            }
        }

        return dist;
    },

    /**
     * Calculate the pearson correlation between a and b.
     * a and b are two 1d arrays of equal length
     */
    correlation: function(a, b) {
        // NOTE: using math.js

        a_mean = math.mean(a);
        b_mean = math.mean(b);

        a_std = math.std(a, 'uncorrected');
        b_std = math.std(b, 'uncorrected');

        // cov(x,y) = E[(x - mu_x)(y - my_y)]
        var covariance = math.mean(
            math.dotMultiply(math.subtract(a, a_mean),
                math.subtract(b, b_mean))
        );

        // rho = cov(x,y) / ( rho_x * rho_y
        var correlation = covariance / (a_std * b_std);

        return correlation;
    },

    /**
     * Generate a sequence of integers
     * @param start
     * @param end
     * @param step
     * @return array of sequence
     */
    seq: function(start, end, step) {
        step = (typeof step !== 'undefined') ? step : 1;
        var r = [];

        if (step > 0) {
            var j = 0;
            for (var i = start; i <= end; i = i + step) {
                r[j] = i;
                j++;
            };
        } else if (step < 0) {
            var j = 0;
            for (var i = start; i >= end; i = i + step) {
                r[j] = i;
                j++;
            };
        } else {
            throw new Error('Step can not be 0');
        }

        return r;
    },
    
    DEproxydataToCSV: function(proxydata) {
      if(proxydata.length == 0) {
        return "";
      }
      
      var sep = ',';
      // generate header
      var csvFile = "name,M,Z,absZ\r\n"
      for (var i = 0; i < proxydata.length; i++) {
        csvFile += proxydata[i].name + sep + proxydata[i].M +sep + 
            proxydata[i].Z + sep + proxydata[i].absZ + '\r\n';
      }
      
      return csvFile;
    }
}
