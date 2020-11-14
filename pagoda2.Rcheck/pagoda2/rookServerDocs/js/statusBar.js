"use strict";

/**
 * Responsible for the status bar display item.
 * @description On initialisation it registers various event handlers
 * to display things. Other object can directly call its showMessage()
 * function to display items.
 * @constructor
 * @todo Minimum time that the displayed item will stay on screen
 * @todo implement icons (i.e. cell, warning, error, ...)
 */
function statusBar() {
    if (typeof statusBar.instance === 'object') {
	return statusBar.instance;
    };

    statusBar.instance = this;
};

/**
 * Update the message in the status bar
 */
statusBar.prototype.showMessage =  function(msg) {
	var extJsStatusBar = Ext.getCmp('pagoda-status-bar');
	extJsStatusBar.setStatus({
	    text: msg
	});
};

