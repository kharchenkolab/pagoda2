"use strict";

/*
 * Filename: clickableRegions.js
 * Author: Nikolas Barkas
 * Date: March 2017
 */

/**
 * Implements a clickable regions on some surface
 * @constructor
 * @description provides support for handling clickable regions on a
 * surface by allowing region registration and resolution.
 * @todo  This is an reimplementation of the dendrogram clickable regions that can be
 * used independently. The dendrogram should be modified to use this
 * and region resolution optimisations should be implemented here
 */
function clickableRegions() {
    this.clickAreas = [];

    this.clickMapCacheArray = [];
    this.clickMapCacheValid = false;
}


/**
 * Resolve a click
 * @description Find which element was clicked and call the callback function
 * with information about it

 */
clickableRegions.prototype.resolveClick = function (x,y, callback) {
        if(this.clickMapCacheValid === false) {
	    return this.resolveClickManual(x,y,callback);
	} else {
	    return this.resolveClickCache(x,y, callback);
	}
}

/**
 * resolve clicks manually by iterating throught the click
 * area array, as opposed to using the cache. Called by resolveClick()
 * @private
 * @param x the x coordinate
 * @param y the y coordinate
 * @param callback optional callback if not defined the data is returned
 * @return the data associated with the region at the time it was created. Returned
 * only if a callback is not specified
 */
clickableRegions.prototype.resolveClickManual =  function (x,y, callback) {
    for (var i = 0; i < this.clickAreas.length; i++) {
	var vs = this.clickAreas[i].vertices
	if (pointInPolygon([x,y], vs)) {
	    if ( typeof callback !== 'undefined' ) {
		callback(this.clickAreas[i].data);
	    } else {
		return this.clickAreas[i].data;
	    }
	    break;
	}; // if
    }; // for
}

clickableRegions.prototype.resolveClickCache = function(x,y, callback) {
    throw new Error('clickableRegions.resolveClickCache() not implemented');
}

/**
 * Clear click areas and invalidate the cache
 */
clickableRegions.prototype.clearClickAreas = function() {
    //var dv = new dendrogramViewer();
    this.clickAreas.length = 0;
    // Invalidate the clickMapCache
    this.clickMapCacheValid = false;
}

/**
 * This function will build the click cache and for every location
 * on the canvas will pre-calculate what needs to be returned.
 * @description NOT IMPLEMENTED. It will also map nearby clicks to regions even if the regions is not
 * specified provided that there are no collisiong. NOTES on future implementation: implement this using a region
 * growing algorith -- take  from image processing. Store the region resolutions
 * parameters seperately as many adjacent pixes will have the same
 * resolution params and no params << no of pixels
 */
clickableRegions.prototype.buildClickCache = function() {
    // TODO: get  this to work
    // Move the region padding here
    console.warn("Click cache building not implemented");
}

/**
 * Add a new click area bounded by four points
 * @description: It is important that click area addition is done through
 * here and not manually so as to allow invaludation of the click area
 * preprocess cache once this is implemented
 * @param x1 x coordinate of point 1
 * @param y1 y coordinate of point 1
 * @param x2 x coordinate of point 2
 * @param y2 y coordinate of point 2
 * @param x3 x coordinate of point 3
 * @param y3 y coordinate of point 3
 * @param x4 x coordinate of point 4
 * @param y4 y coordinate of point 4
 * @param data data associated with this click area that will be passed to the callback on resolution
 */
clickableRegions.prototype.addClickArea = function(x1, y1, x2, y2, x3, y3, x4, y4, data) {
    var clkArea = new Object();
    clkArea["vertices"]  = [ [x1, y1],[x2, y2],[x3, y3],[x4, y4] ];
    clkArea["data"] = data;

    this.clickAreas[this.clickAreas.length] = clkArea;

    // Invalidate the clickMapCache
    this.clickMapCacheValid = false;
}
