"use strict";

/*
 * Filename: paletteManager.js
 * Author: Nikolas Barkas
 * Date: March 2017
 * Description: Manages color palettes
 */

/**
 * Manages color palettes for pagoda
 */
function paletteManager() {
    this.currentPaletteName = null;
    this.currentNumberOfColors = null;

    // TODO: We want a deep copy actually
    this.availablePalettes = p2globalParams.heatmapViewer.availablePalettes;
}

/**
 * Get the available palettes
 */
paletteManager.prototype.getAvailablePalettes = function() {

    return this.availablePalettes;
}

/**
 * Set the current palette name
 * @descriptions verifies and updates the number of colors
 */
paletteManager.prototype.setPalette = function(newPal) {

    // TODO: Check it  is a valid palette
    this.currentPaletteName = newPal;

    // Set the number of colors again to ensure
    // it is valid for this palette
    var n = this.getNumberOfColors();
    this.setNumberOfColors(n);
}

/**
 * Get the current palette Name
 */
paletteManager.prototype.getPaletteName = function() {
    return this.currentPaletteName;
}


/**
 * Returns a function that will do color mapping from
 * a specified range to a palette index
 */
paletteManager.prototype.getMeanClampedColorMapper = function(rowMean, maxAbsValue, palSize) {
    const trim = 0.3;
    maxAbsValue = maxAbsValue * trim;

    if (rowMean == 0 && maxAbsValue == 0) {
      return function(v) {return 0};

    } else {

      return function(v) {
      	var plotValue = (v - rowMean) / (maxAbsValue * 2) + 0.5;
      	var palIndex = Math.floor(plotValue * palSize) - 1;
      	if (palIndex < 0) {
      	    palIndex = 0;
      	} else if (palIndex > palSize - 1) {
      	    palIndex = palSize -1;
      	}

  	    return palIndex;
      }

    }
}

/**
 * Set the number of colors
 */
paletteManager.prototype.setNumberOfColors = function(v) {
    // TODO: check v is a number

    var palName = this.getPaletteName();
    // TODO: Fix me
    var palEntry = this.availablePalettes[palName];
    if (v > palEntry.maxColors) {
	    v = palEntry.maxColors;
    } else if ( v < palEntry.minColors ) {
	    v = palEntry.minColors;
    }

    this.currentNumberOfColors =  v;
}

/**
 * Get teh maximum number fo colors for the current palette
 */
paletteManager.prototype.getMaxNumberOfColors = function() {
  var palName = this.getPaletteName();
  return this.availablePalettes[palName].maxColors
}

/**
 * Get the minimum number fo colors for the current palette
 */
paletteManager.prototype.getMinNumberOfColors = function() {
  var palName = this.getPaletteName();
  return this.availablePalettes[palName].minColors
}

/**
 * Get the number of colors currently set
 */
paletteManager.prototype.getNumberOfColors = function() {
    return this.currentNumberOfColors;
}

/**
 * Generate colors given the current palette settings
 * @return array of colors
 */
paletteManager.prototype.getPaletteColors = function() {
    var palName = this.getPaletteName();
    var palSize = this.getNumberOfColors();

    // TODO: Move this out of heatmap viewer and also make a in object copy
    var palEntry = this.availablePalettes[palName];

    // Different palette generation methods
    if (palEntry.generate === 'palette.js-standard') {
	var pal = palette(palName, palSize);
	pal = pal.map(function(x){return '#' + x});
	return pal;
    } else if ( palEntry.generate === 'fixed-values') {
	var pal = palEntry.colorValues;
	return pal;
    } else {
	console.warn('Unknown palette generation method: ' + palEntry.generate);
	return ['#000000'];
    }
}



