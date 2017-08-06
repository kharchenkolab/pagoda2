"use strict";

/*
 * Filename: geneSelectionController.js
 * Author: Nikolas Barkas
 * Date: March 2017
 */

/**
 * Manage gene selections
 * @constructor
 */
function geneSelectionController() {
    if (typeof geneSelectionController.instance === 'object') {
	    return geneSelectionController.instance;
    }

    this.selections = new Object();
    this.idNum = 0;
    geneSelectionController.instance = this;
}

/**
 * Set a gene selection
 * @param {string} selectionName The gene selection name
 * @param {Array[]} genes The gene identifers for this selection
 */
geneSelectionController.prototype.setSelection = function(genes, displayName, selectionName) {


    if ( typeof displayName === 'undefined') {
	    displayName = selectionName;
    }
    if(typeof selectionName === "undefined"){
      selectionName = this.idNum + "_" + (new Date()).getTime();
      this.idNum++;
    }
    else{
      selectionName = "auto_" + selectionName;
    }

    this.selections[selectionName] = ({
    	'name': selectionName,
    	'genes': genes,
    	'displayName': displayName
    });

    //this.selections[selectionName] = genes;
    this.raiseSelectionChangedEvent();
};

/**
 * Get the display name for this selection
 * @param selectionName the internal name fo the selection
 */
geneSelectionController.prototype.getSelectionDisplayName = function(selectionName) {
    var sel = this.selections[selectionName];
    if (typeof sel != 'undefined')  {
	return sel.displayName;
    } else {
	return undefined;
    }
};

/**
 * Raise a global event that the selection has been changed
 * @private
 */
geneSelectionController.prototype.raiseSelectionChangedEvent = function() {
    var evtBus = new eventBus();
    evtBus.publish("gene-selection-updated");
}

/**
 * Get a gene selection
 * @param {string} selectionName The gene selection name
 * @returns {Array[]} The genes in this selection
 */
geneSelectionController.prototype.getSelection = function(selectionName) {
    return this.selections[selectionName];
};

/**
 * Clear a gene selection
 * @param {String} selectionname The name of the selection to clear
 */
geneSelectionController.prototype.clearSelection = function(selectionname) {
    this.selections[selectionName] = [];
    var evtBus = new eventBus();
    evtBus.publish("gene-selection-cleared");
};

/**
 * Checks to see if a cell selection contains a given displayName
 * @param {string} dispName the name we are looking for
 * @returns {boolean} whether or not dispName exists among all other displayNames
 */
geneSelectionController.prototype.displayNameExists = function(dispName){
  for(var sel in this.selections){
    if(this.selections[sel].displayName === dispName){
      return true;
    }
  }
  return false;
}

/**
 * Get the names of the currently available selections
 */
geneSelectionController.prototype.getAvailableSelections = function() {
    var names = [];

    for (var sel in this.selections) {
	names.push(this.selections[sel].name);
    }

    return names;
};

/**
 * Delete a gene selection
 * @param {string} selectionName the name of the selection to delete
 */
geneSelectionController.prototype.deleteSelection = function(selectionName) {
    delete this.selections[selectionName];
    this.raiseSelectionChangedEvent();
}

/**
 * Duplicate an existing selection
 */
geneSelectionController.prototype.duplicateSelection = function(selectionName, newSelectionDisplayName) {
    var oldSelection = this.selections[selectionName];
    this.setSelection(JSON.parse(JSON.stringify(oldSelection.genes)),newSelectionDisplayName);
}

/**
 * Change the display name of the given gene selection
 */
geneSelectionController.prototype.renameSelection = function(selectionName, newSelectionName) {
    this.selections[selectionName].displayName = newSelectionName;
    this.raiseSelectionChangedEvent();
}

/**
 * Generate a new gene selection from two existing gene selections
 */
geneSelectionController.prototype.mergeSelectionsIntoNew = function(selections, newSelectionDisplayName)  {

    var geneSelCntrl = this;
    var genes = {};
    selections.forEach(function(selection){
      geneSelCntrl.selections[selection].genes.forEach(function(gene){
        genes[gene] = true;
      })
    });

    geneSelCntrl.setSelection(Object.keys(genes), newSelectionDisplayName);
}

/**
 * Genereate a new gene selection by intersecting two gene selections
 */
geneSelectionController.prototype.intersectSelectionsIntoNew = function(selections, newSelectionDisplayName){
    var geneSelCtrl = this;

    var genes = {};
    geneSelCtrl.selections[selections[0]].genes.forEach(function(gene){
      genes[gene] = 1;
    });
    for(var i = 1; i< selections.length; i++){
      geneSelCtrl.selections[selections[i]].genes.forEach(function(gene){
        if(genes[gene]){
          genes[gene]++;
        }
      });
    }
    var puregenes = [];
    for(var gene in genes){
      if(genes[gene] === selections.length){
        puregenes.push(gene);
      }
    }
    this.setSelection(puregenes, newSelectionDisplayName)
}

/**
 * Genereate a new gene selection by complimenting one or more gene selections
 */
geneSelectionController.prototype.complimentSelectionsIntoNew = function(selections, newSelectionDisplayName){

  var geneSelCtrl = this;
  var genes = {};
  for(var i = 0; i< selections.length; i++){
    var selection = selections[i];
    geneSelCtrl.getSelection(selection).genes.forEach(function (gene){
      genes[gene] = true;
    });
  }

  (new dataController()).getGeneInformationStore(function(store){
    var genesPrime = [];
    var data = store.localData;
    for(var gene in data){
      if(!genes[data[gene].genename]){
        genesPrime.push(data[gene].genename);
      }
    }
    geneSelCtrl.setSelection(genesPrime, newSelectionDisplayName)
  })
}
/**
 * Genereate a new gene selection by finding the difference between two or more gene selections
 */
geneSelectionController.prototype.differenceSelectionsIntoNew = function(selections, newSelectionDisplayName){
  var geneSelCtrl = this;
  var genes = {}
  for(var i = 0; i< selections.length; i++){
    var selection = selections[i];
    geneSelCtrl.getSelection(selection).genes.forEach(function(gene){
      if(!genes[gene]){genes[gene] = 0}
      genes[gene]++;
    })
  }
  var geneIntersect = [];
  for(var gene in genes){
    if(genes[gene] === 1){
      geneIntersect.push(gene);
    }
  }
  geneSelCtrl.setSelection(geneIntersect,newSelectionDisplayName);
}
