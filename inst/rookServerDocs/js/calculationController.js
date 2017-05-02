
/**
 * Perform compulations on the data
 * @constructor
 */
function calculationController(localAvailable, remoteAvailable) {
    if ( typeof calculationController.instance === 'object' ){
	    return calculationController.instance;
    };

    this.methods = [
      {
        name: 'remoteDefault',
        displayName: 'Remote Default',
        help: 'Remote Default Method',
        repos: 'remote'
      },
      {
        name: 'localDefault',
        displayName: 'Local Default',
        help: 'Local default method',
        repos: 'T-test'
      }
    ];

    calculationController.instance = this;
    return this;
}

calculationController.prototype.calculateDE = function(selectionA, selectionB, method, callback) {
  callback('Not implemented');
}

calculationController.prototype.getAvailableDEmethods = function() {
  return this.methods;
}


/**
 * @constructor
 */
function calculationControllerLocal() {

}

/**
 * @constructor
 */
function calculationControllerRemote() {


}
