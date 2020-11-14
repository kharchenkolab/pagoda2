"use strict";

/*
 * Filename: eventBus.js
 * Author: Nikolas Barkas
 * Date: March 2018
 */

/**
 * implements the main event bus
 * @constructor
 * @description it is used to communicate between
 * different components on the page without having to create
 * binding code. Implements Singlenton.
 * @example
     // Example usage
     var evtBus = new eventBus();

     // Register an event handler
     evtBus.register("customEventIdentifier",
                     {info: "Some information From Registration Time"},
		     function(event, params) {
		           // Registation information in e.data
			   // Publish information in params
		     });
     // Raise an event
     evtBus.publish("customEventIdentifier",
                    {info: "Some information from publicationtime"});
 */
function eventBus() {
    if (typeof eventBus.instance === 'object') {
	return eventBus.instance;
    }

    eventBus.instance = this;
};

/**
 * register an event
 */
eventBus.prototype.register = function(event, data, handler) {
	$(this).on(event, null, data, handler);
};

/**
 * unregister an event
 */
eventBus.prototype.unregister = function(event, data, handler) {
	$(this).off(event, data, handler);
};

/**
 * publish an event
 */
eventBus.prototype.publish = function(event, params) {
	$(this).trigger(event, params);
};

/*
 * List of events used in the app for reference
 *    embedding-mouseover-plotpoint
 *    embedding-click-plotpoint
 *
 *    genetable-item-selected
 *    genetable-item-deselected
 *
 *    pathwaytable-item-selected
 *    pathwaytable-item-deselected
 *
 *    dendrogram-node-click
 *
 *    cell-selection-updated
 *    cell-selection-cleared
 *
 *    dendrogram-update-complete
 */

