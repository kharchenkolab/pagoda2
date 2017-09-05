"use strict";

/**
 * Stores differential expression results and accompanying metadata
 * Singleton
 * @constructor
 */
function differentialExpressionStore() {
    if ( typeof differentialExpressionStore.instance === 'object' ){
	    return differentialExpressionStore.instance;
    };

    this.deSets = new Object();

    differentialExpressionStore.instance = this;
    return this;
};

/**
 * Get all available differential expression sets
 */
differentialExpressionStore.prototype.getAvailableDEsets = function() {
  var result = new Array();
  var availKeys = Object.keys(this.deSets);

  // Push key/display name pairs on an array
  for (var i = 0; i < availKeys.length; i++) {
    var curKey = availKeys[i];
    var name = curKey;
    var displayName = this.deSets[curKey].getName();

    var date = this.deSets[curKey].getStartTime().valueOf();
    result.push({'name': name, 'date': date, 'displayName': displayName});
  }
  return result;
};

/**
 * Get a de result by internal name
 */
differentialExpressionStore.prototype.getResultSetByInternalName = function(internalName) {
  //TODO: Implement some checks here
  return this.deSets[internalName];
}


/**
 * Add differential expression result to the store
 */
differentialExpressionStore.prototype.addResultSet = function(deset) {
  var newid = this.getUniqueId();
  this.deSets[newid] = deset;
  return newid;
};


/**
 * Generate a unique identifier for a de set
 * @private
 */
differentialExpressionStore.prototype.getUniqueId = function() {
  var dobj = new Date();
  var r = Math.floor(Math.random() * 1e12);
  var d = dobj.getTime();
  return  'deset_' +  d + '_' + r
}

