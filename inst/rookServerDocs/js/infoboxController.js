
/**
 * Manages the information box and its content
 * @constructor
 */
function infoboxController() {
    if (typeof infoboxController.instance === 'object') {
	return infoboxController.instance;
    }   

    var evtBus = new eventBus();

    console.log("Initializing infobox controller...");

    // Generate the table and a store for it
    var infoAppContainer = Ext.getCmp('information-app-container');
    var messageTableStore = Ext.create('Ext.data.Store', {
	id: 'messageTableStore',
	sortable: false,
	defaultSortable: false,
	fields: [{name: 'message', type: 'string'}],
    });
    
    var msgTable = Ext.create('Ext.grid.Panel', {
	store: Ext.data.StoreManager.lookup('messageTableStore'),
	columns: [{ text: 'Messages', dataIndex: 'message', width: '100%', sortable: false }],
	width: '100%',
	height: '100%',
	scrollable: true
    });

    infoAppContainer.add(msgTable);

    // Register Events to update the Messages as required here

    // Report update of cell seletion
    evtBus.register("cell-selection-updated", null, function() {
	var selCntr = new cellSelectionController();
	var primarySel = selCntr.getSelection('currentPrimarySel');
	var secondarySel = selCntr.getSelection('currentAltSel');

	var infBoxCntr = new infoboxController();
	infBoxCntr.addMessage('--- Selection Updated ---' );
	if ( typeof primarySel != 'undefined' ) 
	    infBoxCntr.addMessage('Primary cell selection: ' + primarySel.length);

	if( typeof secondarySel !== 'undefined')
	    infBoxCntr.addMessage('Secondary cell selection: ' + secondarySel.length);
    });

    // Singleton stuff
    infoboxController.instance =  this;
}

/**
 * Add a message to the panel
 */
infoboxController.prototype.addMessage = function(msg) {
    var store = Ext.data.StoreManager.lookup('messageTableStore')
    store.add({message: msg});
}
