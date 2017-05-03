


//////////////////////////////////////////////////

/**
 * Manages the action panel interface. Singleton
 * @constructor
 */
function actionPanelUIcontroller() {
    if (typeof actionPanelUIcontroller.instance === 'object') {
	return actionPanelUIcontroller.instance;
    }

    this.generateCellSelectionStore();
    this.generateUI();

    // Setup listener for selection change
    var evtBus = new eventBus();
    evtBus.register("cell-selection-updated", null, function() {
	var actPaneUICntr = new actionPanelUIcontroller();
	actPaneUICntr.syncCellSelectionStore();
    });

    actionPanelUIcontroller.instance = this;
};


/**
 * Generate the UI controls
 * @private
 */
actionPanelUIcontroller.prototype.generateUI = function() {
    var actionsTab = Ext.getCmp('actions-ui-tab');

    var actionsInnerTab = Ext.create('Ext.TabPanel', {
	layout: 'fit',
	width: '100%',
	height: '100%',
	tabBarPosition: 'top',
	items: [
	    {
		layout: 'fit',
		title: 'Differential Expression',
		id: 'differentialExpressionTab',
		glyph: 0xf14e, //fa-compass
		tooltip: 'Perform differential expression between two groups of cells',
		height: '100%',
		width: '100%',
	    },
	    {
		layout: 'fit',
		title: 'Enrichment',
		glyph: 0xf200, // pie chart
		tooltip: 'Perform enrichment analysis',
		height: '100%',
		width: '100%'
	    }
	]
    });

    // Data store for the available diff expr methods
    var deMethodsStore = Ext.create('Ext.data.Store', {
	id: 'availableDEmethodStore',
	fields: [{name: 'name', type: 'string'},
	       {name:'displayname', type: 'string'}]
    });

    // Populate the store
    var calcCntr = new calculationController();
    var availableMethods = calcCntr.getAvailableDEmethods();


    for (i in availableMethods) {
	    deMethodsStore.add({name: availableMethods[i].name, displayname: availableMethods[i].displayName});
    }

    var deTab = Ext.getCmp('differentialExpressionTab');

    var formPanelDE =  Ext.create('Ext.form.Panel', {
	id: 'formPanelDE',
	height: '100%',
	width: '100%',
	bodyPadding: 10,
	items: [{
	    id: 'selectionA',
	    xtype: 'combo',
	    fieldLabel: 'Cell Selection A',
	    queryMode: 'local',
	    editable: false,
	    store: Ext.data.StoreManager.lookup('cellSelectionStoreForDE'),
	    displayField: 'displayname',
	    valueField: 'selectionname'
	},
	{
	    id: 'selectionB',
	    xtype: 'combo',
	    fieldLabel: 'Cell Selection B',
	    queryMode: 'local',
	    editable: false,
	    store: Ext.data.StoreManager.lookup('cellSelectionStoreForDE'),
	    displayField: 'displayname',
	    valueField: 'selectionname'
	},{
	    id: 'selectionMethod',
	    xtype: 'combo',
	    fieldLabel: 'Method', // TODO add help pane
	    queryMode: 'local',
	    editable: 'false',
	    store: Ext.data.StoreManager.lookup('availableDEmethodStore'),
	    displayField: 'displayname',
	    valueField: 'name'
	},{
	  id: 'resultName',
	  xtype: 'textfield',
	  fieldLabel: 'Name for results'
	},{
	    xtype: 'button',
	    text: 'Run differential expression',
	    handler: function() {
		var selectionA = Ext.getCmp("formPanelDE").getForm()
		    .findField("selectionA").getValue();
		var selectionB = Ext.getCmp("formPanelDE").getForm()
		    .findField("selectionB").getValue();

		if (selectionA === selectionB) {
		    Ext.MessageBox.alert('Warning', 'Please select a different set for A and B');
		} else {
		    //TODO: Some kind of visual wait indicator
		    var calcCntr = new calculationController();
		    calcCntr.calculateDEbySelection(selectionA, selectionB, 'remoteDefault',  function(results) {
		        console.log('We are in the callback', results)

		        // TODO: Take the results and put them in a a table for diff expressinpo



		    } );
		}
	    }
	}
	] //items
    });
    deTab.add(formPanelDE);


    actionsTab.add(actionsInnerTab);
};

/**
 * Generate the cell selection store for populating the dropdowns
 * @private
 */
actionPanelUIcontroller.prototype.generateCellSelectionStore = function() {
    Ext.create('Ext.data.Store', {
	storeId: 'cellSelectionStoreForDE',
	fields: [
		{ name: 'selectionname', type: 'string'},
		{ name: 'displayname', type: 'string'}
	],
	autoLoad: true
    });
}


/**
 * Update the cell selection store for populating the dropdowns
 * with information from the selection controller
 * @private
 */
actionPanelUIcontroller.prototype.syncCellSelectionStore = function() {
    // Get the store for the table and empty it
    var store = Ext.data.StoreManager.lookup('cellSelectionStoreForDE');
    store.removeAll();

    // Repopulate the store
    var cellSelCntr =  new cellSelectionController();
    var availSelections = cellSelCntr.getAvailableSelections();

    for (sel in availSelections) {
    	var selName = availSelections[sel];
    	var selDisplayName =  cellSelCntr.getSelectionDisplayName(selName);

    	store.add({selectionname: selName, displayname: selDisplayName});
    }// for

}
