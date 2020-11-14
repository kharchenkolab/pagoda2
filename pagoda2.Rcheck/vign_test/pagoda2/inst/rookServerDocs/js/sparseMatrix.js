"use strict";

/**
 * Filename: sparseMatrix.js
 * Author: Nikolas Barkas
 * Date: February 2017
 * Description: Reader for dgCMatrix from R
 */

/**
 * A read-only compressed sparse row matrix
 * @constructor
 */
function dgCMatrixReader(i, p, Dim, Dimnames1, Dimnames2, x, factors) {
    this.i =  i;
    this.p =  p;
    this.Dim = Dim;
    this.DimNames1 = Dimnames1;
    this.DimNames2 = Dimnames2;
    this.x = x;
    this.factors = factors;
}

/**
 * Get a full js matrix out of the sparse matrix representation
 */
dgCMatrixReader.prototype.getFullMatrix = function() {

    // Make a zero filled array
    var out = Array(this.Dim[0]);
    for (var k = 0; k < this.Dim[0]; k++) {
    	var row = Array(this.Dim[1]);
    	for (var j = 0; j < this.Dim[1]; j++) {
    	    row[j] = 0;
    	}
    	out[k] = row;
    }


    // index in p (the column number)
    for (var j = 0; j < this.p.length - 1; j++) {
    	// row start index, row end index (in x and i)
    	var rsi = this.p[j];
    	var rei = this.p[j+1] - 1;

    	// k is an index in x and i with the current column
    	for (var k = rsi; k < rei; k++) {
    	    // row number
    	    var rn = this.i[k];

    	    // x[k] is the value for the element in rn, j
    	    out[rn][j] = this.x[k];
    	}
    }

    var retVal = {
	array:  out,
	rownames:  this.DimNames1,
	colnames:  this.DimNames2
    };

    return retVal;
}


/**
 * Get a full js matrix out of the sparse matrix representation
 */
dgCMatrixReader.prototype.getFullMatrixTransposed = function() {

    // Make a zero filled array
    var out = Array(this.Dim[1]);
    for (var k = 0; k < this.Dim[1]; k++) {
	var row = Array(this.Dim[0]);
	for (var j = 0; j < this.Dim[0]; j++) {
	    row[j] = 0;
	}
	out[k] = row;
    }


    // index in p (the column number)
    for (var j = 0; j < this.p.length - 1; j++) {
	// row start index, row end index (in x and i)
	var rsi = this.p[j];
	var rei = this.p[j+1] - 1;

	// k is an index in x and i with the current column
	for (var k = rsi; k < rei; k++) {
	    // row number
	    var rn = this.i[k];

	    // x[k] is the value for the element in rn, j
	    out[j][rn] = this.x[k];
	}
    }

    var retVal = {
	array:  out,
	rownames:  this.DimNames2,
	colnames:  this.DimNames1
    };

    return retVal;
}
