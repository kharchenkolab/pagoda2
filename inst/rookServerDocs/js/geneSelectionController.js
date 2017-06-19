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
    };

    this.selections = new Object();

    geneSelectionController.instance = this;
};

/**
 * Set a gene selection
 * @param {string} selectionName The gene selection name
 * @param {Array[]} genes The gene identifers for this selection
 */
geneSelectionController.prototype.setSelection = function(selectionName, genes, displayName) {
    if ( typeof displayName === 'undefined') {
	displayName = selectionName;
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
geneSelectionController.prototype.duplicateSelection = function(selectionName,
								newSelectionName,
								newSelectionDisplayName) {
    var oldSelection = this.selections[selectionName];
    var sel = {};
    sel.name = newSelectionName;
    sel.displayName = newSelectionDisplayName;

    sel.genes = JSON.parse(JSON.stringify(oldSelection.genes));

    this.selections[newSelectionName] = sel;
    this.raiseSelectionChangedEvent();
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
geneSelectionController.prototype.mergeSelectionsIntoNew = function(selections, newSelectionName, newSelectionDisplayName)  {

    var geneSelCntrl = this;
    var sel = {};
    sel.name = newSelectionName;
    sel.displayName = newSelectionDisplayName;

    var genes = {};
    selections.forEach(function(selection){
      geneSelCntrl.selections[selection].genes.forEach(function(gene){
        genes[gene] = true;
      })
    });
    sel.genes = Object.keys(genes);
    
    geneSelCntrl.selections[newSelectionName] = sel;
    geneSelCntrl.raiseSelectionChangedEvent();
}

/**
 * Genereate a new gene selection by intersecting two gene selections
 */
geneSelectionController.prototype.intersectSelectionsIntoNew = function(selectionA, selectionB, newSelectionName, newSelectionDisplayName){
    var selA = this.selections[selectionA].genes;
    var selB = this.selections[selectionB].genes;

    var sel = {};
    sel.name = newSelectionName;
    sel.displayName = newSelectionDisplayName;
    sel.genes = [];

    // TODO: There might be performance benefits in looping over shortere array
    var l = selA.length;
    for (var i = 0; i < l; i++){
      if (selB.indexOf(selA[i]) != -1) {
        sel.genes.push(selA[i]);
      }
    }

    this.selections[newSelectionName] = sel;
    this.raiseSelectionChangedEvent();
}
