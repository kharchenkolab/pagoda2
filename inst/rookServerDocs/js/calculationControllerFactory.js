
/**
 * Factory class for calculation controller
 * @description the calculation controller does not have to be a singleton
 * but all the objects need to be intialised with the repository
 * parameters specified in the initialise() function. This factory is 
 * a singleton that will do this while allowing multiple calculationController 
 * objects to coexist
 * @constructor
 * @repos {string} repository type
 */ 
function calculationControllerFactory(repos) {
    if ( typeof calculationControllerFactory.instance === 'object' ){
	return calculationControllerFactory.instance;
    }

    this.repository  = repos;

    calculationControllerFactory.instance = this;
}

calculationControllerFactory.prototype.makeCalculationController = function() {
   return new calculationController(this.repository, true);
}

////////////////////////

/** 
 * Perform compulations on the data either server or client side
 * @description DO NOT INITIALIZE DIRECTLY USE CALCULATION CONTROLLER FACTORY
 * @param repos {string} the calculation repository (?) local or remote
 * @param initialisedByFactory dummy variable to prevent directy initialisatioin
 * @constructor
 */
function calculationController(computeEngineType, initialisedByFactory) {
    if (initialisedByFactory !== true) {
	throw('Calculation controller cannot be initialised directly! Use calculationControllerFactory.');
    }

    this.computeEngine = null;

    if (computeEngineType === 'local') {
	this.computeEngine =  new localComputeEngine();
    } else if (computeEngineType === 'remote') {
	this.computeEngine = new remoteComputeEngine();
    }  else {
	console.error("Unknown compute engine type: " + computeEngineType);
    }
}

/** 
 * Perform differential expression giver two gene sets A and B
 * @param selectionA {string} internal name of selection A
 * @param selectionB {string} internal name of selection B
 * @param callback callback function to call when computation complete
 */
calculationController.prototype.performDEbySelection = function (selectionA, selectionB, method, callback) {
    // TODO: Resolve cell sets
    var cellSetA = [];
    var cellSetB = [];

    this.computeEngine.performDE();
}

/**
 * Return a list of available methods for differential expression currently available
 */
calculationController.prototype.getAvailableDEmethods = function() {
    // Ask the selected compute engine
    return this.computeEngine.getAvailableDEMethods();
}
//////////////////////////////////////////

/**
 * Engine for performing local computations including de and enrichment
 * @constructor
 * @description it is ok to generate instances of this for operations that
 * will be done locally. If the operation should be delegated to the remote
 * computation engine get a calculation controller from the the calculation controller
 * factory instead
 */
function localComputeEngine() {
    // NOTHING TO DO
}

/**
 * Get the available methods for differential expression supported
 * by this engine
 */
localComputeEngine.prototype.getAvailableDEMethods = function() {
    var supportedMethods = [
	{
	    name: 'ttestfdr',
	    displayName: 'FDR corrected T-test',
	    help: 'Some help text for the user'
	},
	{
	    name: 'ttestbonferroni',
	    displayName: 'Bonferroni corrected T-test',
	    help: 'Some help text for the user'
	}
    ];

    return supportedMethods;
}


/**
 * Perform differential expression locally using js
 */
localComputeEngine.prototype.performDE = function(cellSetA, cellSetB, method) {
    console.error('Perform DE by local compute engine not implemented');
}

/**
 * Winsorize Matrix row by row
 * @description winsorize matrix row by row clipping 'trim' extreme values
 * this implementation is rather slow, but it does generate deep copy of the matrix
 * @parameter matrix a 2d matrix with all rows of equal lenght
 * @parameter trim percent of extreme values to clip from both ends
 * @parameter skipColumns number of columns to skip on the right
 * @returns the winsorized matrix
 * @obsolete 
 */
localComputeEngine.prototype.winsorizeMatrix = function(matrix, trim, skipColumns) {
    var retMatrix = [];

    var k = matrix.length; // n rows
    var n = matrix[0].length; // n cols

    // Number to trim
    var ntr = Math.round(n * trim);
    
    // Row
    for ( var i = 0 ; i < k; i++) {
	var row = matrix[i];
	var newRow = [];

	var rowOrdered = row.slice();
	rowOrdered.sort();
	
	// Set limits
	var minv = rowOrdered[ntr] ;
	var maxv = rowOrdered[n - ntr -1];

	for (var j = 0; j < n; j++) {
	    if (j < skipColumns) {
		// skipColumns -- these are usually labels
		newRow[j] = row[j];;
	    } else {
		if (row[j] < minv) {
		    newRow[j] =  minv;
		} else if (row[j] > maxv) {
		    newRow[j] = maxv;
		} else {
		    newRow[j] = row[j]
		}
	    }
	} // for var j

	retMatrix[i] = newRow;
    }

    return retMatrix;
}


//////////////

/**
 * Engine for performing remote computations
 * @constructor
 */
function remoteComputeEngine() {
    console.error('Remote compute engine not implemented!');
}



