"use strict";
/*
 *
 * The selection cotroller is responsible for keeping
 * track of cell selections. The selections are kept as
 * names objects in an array and they are themselves arrays of
 * text labels of cells
 *
 * Some selection names have special meanings, but this is
 * not otherwise enforced:
 *
 * currentPrimarySel -- the current selection of cells
 * currentSecondarySel -- the current secondary selection of cells
 *                        when one is needed, such as when selecting
 *                        two dendrogram branches
 *
 */

/**
 * Keeps track of cell seletions
 * @constructor
 */
function cellSelectionController() {
    if (typeof cellSelectionController.instance === 'object') {
	return cellSelectionController.instance;
    }

    // Generat the array to keep the selections in
    this.selections = new Object();
    this.highlights = ["#FF0097","#A200FF","#00ABA9","#8CBF26","#A05000","#E671B8","#F09609","#1BA1E2","#E51400","#339933","#FFFF00","#8F0012","#E30181","#B3773C","#C76306","#ABDBB0","#8F0012","#FADC50","#E490B5","#AFD0F4","#8ED0BB","#39FA61","#B56194","#8DDE06","#C1BC76"];
    cellSelectionController.instance = this;
};

/**
 * Sets a cell selection
 * @param {string} selectionName A name for this cell selection
 * @param {Array[]} cells identifiers of the cells for this selection
 * @param {string} displayName Name to show in the UI when refering to this selection
 * @param {object} metadata any kind of metadata we want to attach to this cell selection
 * @param {color} color of the cells when highlighted. If not specified random color is chosen
 */
cellSelectionController.prototype.setSelection = function(selectionName, cells, displayName, metadata, color ) {
    if(typeof color === "undefined"){
      color = this.highlights[Math.floor(Math.random()* this.highlights.length)];
    }
     if (typeof displayName === 'undefined') {
	    displayName = selectionName;
    }

    this.selections[selectionName] = {
	'name': selectionName,
	'cells': cells,
	'displayName': displayName,
	'metadata': metadata,
	'color' : color
    };
    this.raiseSelectionChangedEvent();
};


/**
 * Raise an event for notifying that the cell selectiion has changed
 * @private
 */
cellSelectionController.prototype.raiseSelectionChangedEvent = function() {
    // TODO: buffer this
      // Notify the world that a new selection has been set
    var evtBus = new eventBus();
    evtBus.publish("cell-selection-updated");
}

/**
 * Get the names of the currently available selections
 */
cellSelectionController.prototype.getAvailableSelections = function() {
    var names = [];
    for (var sel in this.selections) {
	names.push(this.selections[sel].name);
    }

    return names;
};

/**
 * Delete a cell selection
 * @param {string} selectionName the name of the selection to delete
 */
cellSelectionController.prototype.deleteSelection = function(selectionName) {

    delete this.selections[selectionName];
this.raiseSelectionChangedEvent();
}

/**
 * Get a cell selection
 * @param {string} selectionName the internal name of the cell selection
 * @returns {Array[]} Identifiers of the cells in this selection
 */
cellSelectionController.prototype.getSelection = function(selectionName) {
    var sel =  this.selections[selectionName];
    if (typeof sel !== 'undefined') {
	return sel.cells;
    } else {
	return undefined;
    }
};

/**
 * Get a cell selection's highlight color
 * @param {string} selectionName the internal name of the cell selection
 * @returns {string} color corresponding to the selected selection
 */
cellSelectionController.prototype.getColor = function(selectionName){
  var sel =  this.selections[selectionName];
    if (typeof sel !== 'undefined') {
	return sel.color;
    } else {
	return undefined;
    }
};
/**
 * Sets a cell selection's highlight color
 * @param {string} selectionName the internal name of the cell selection
 * @param {string} desired new highlight color
 */
cellSelectionController.prototype.setColor = function(selectionName, newColor){

  this.selections[selectionName].color =  newColor;
  this.raiseSelectionChangedEvent();

}
/**
 * get the display name of a cell selection
 * @param selectionName the internal name of the selection
 * @retuns the display name of the selection
 */
cellSelectionController.prototype.getSelectionDisplayName = function(selectionName) {
    var sel =  this.selections[selectionName];
    if (typeof sel !== 'undefined') {
	return sel.displayName;
    } else {
	return undefined;
    };
};

/**
 * Create a new cell selection by duplicating an existing one
 * @param selectionName {string} name of the selection to duplicate
 * @param newSelectionName {string} name of the new selection to make
 * @param newSelectionDisplayName {string} display name of the new selection
 */
cellSelectionController.prototype.duplicateSelection = function(selectionName,
								newSelectionName,
								newSelectionDisplayName) {
    var oldSelection = this.selections[selectionName];
    var sel = {};

    sel.name = newSelectionName;
    sel.displayName = newSelectionDisplayName;
    sel.color = this.highlights[Math.floor(Math.random()*this.highlights.length)];
    sel.cells = JSON.parse(JSON.stringify(oldSelection.cells));

    this.selections[newSelectionName] = sel;
    this.raiseSelectionChangedEvent();
}

/**
 * Rename a cell selection, only change the display name
 */
cellSelectionController.prototype.renameSelection = function(selectionName, newSelectionName) {
    // TODO check tha the new selection name does not already exist
    this.selections[selectionName].displayName =  newSelectionName;
    this.raiseSelectionChangedEvent();
}


/**
 * Generate a new cell selection from two existing cell selections
 */
cellSelectionController.prototype.mergeSelectionsIntoNew = function(selections, newSelectionName, newSelectionDisplayName)  {

    var cellSelCtrl = new cellSelectionController();
    var sel = {};
    sel.name = newSelectionName;
    sel.displayName = newSelectionDisplayName;
    sel.color = this.highlights[Math.floor(Math.random()*this.highlights.length)];

    var cells = {};
    selections.forEach(function(selection){
      cellSelCtrl.getSelection(selection).forEach(function(cell){
        cells[cell] = true;
      })
    });
    sel.cells = Object.keys(cells);
    
    this.selections[newSelectionName] = sel;
    this.raiseSelectionChangedEvent();
}

/**
 * Genereate a new cell selection by intersecting two cell selections
 */
cellSelectionController.prototype.intersectSelectionsIntoNew = function(selectionA, selectionB, newSelectionName, newSelectionDisplayName){
    var selA = this.selections[selectionA].cells;
    var selB = this.selections[selectionB].cells;

    var sel = {};
    sel.name = newSelectionName;
    sel.displayName = newSelectionDisplayName;
    sel.cells = [];
    sel.color = this.highlights[Math.floor(Math.random()*this.highlights.length)];

    // TODO: There might be performance benefits in looping over shortere arFray
    var l = selA.length;
    for (var i = 0; i < l; i++){
      if (selB.indexOf(selA[i]) != -1) {
        sel.cells.push(selA[i]);
      }
    }

    this.selections[newSelectionName] = sel;
    this.raiseSelectionChangedEvent();
}





