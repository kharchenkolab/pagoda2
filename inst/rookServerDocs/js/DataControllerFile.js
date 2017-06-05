
/**
 * Handles data access by File, either local or remote
 */
function DataControllerFile(loadParams) {
  if(!(loadParams.connectionType == 'remoteFile')) {
    throw new Error('DataControllerFile does not support the selected connection type');
  }

  // TODO: handle local

  // Make a p2file reader with the appropriate settings and save it here
  var frTemp = new p2FileReader('remote', loadParams.remoteFileUrl,null);

  // Generate format reader
  this.formatReader = new p2FormatReader(frTemp);
  this.formatReader.onReady = function() { console.log('On ready fired'); }
  this.formatReader.readHeaderIndex();

}

DataControllerFile.prototype.getReducedDendrogram = function(callback) {
  // Assume format reader is ready here
  var fr = this.formatReader;

  // Actually get dendrogram
  var fn = function() {

        fr.getEntryAsText('reduceddendrogram', function(text) {
      		// Find lenght of null terminated string
      		var dataLength;
      		for (dataLength = 0; dataLength < text.length; dataLength++) {
      		    var c = text.slice(dataLength, dataLength + 1);
      		    if (c.charCodeAt(0) === 0) {
      		      // Found zero
      			    break;
      		    }
      		}
      		var textTrimmed = text.slice(0, dataLength);
      		callback(JSON.parse(textTrimmed));


  	 }, fr);
  }


  // Call immediately or defer to when the object is ready
  if (fr.state == fr.READY) {
    fn();
  } else {
    fr.onReady = fn;
  }
}

DataControllerFile.prototype.getCellOrder = function(callback) {

}

DataControllerFile.prototype.getAvailableAspectsStore = function(callback) {

}

DataControllerFile.prototype.getAvailableGenesetsInAspectsStore = function(aspectId, callback) {

}

DataControllerFile.prototype.getAspectMatrix = function(cellIndexStart, cellIndexEnd, getCellNames, callback) {

}

DataControllerFile.prototype.getExpressionValuesByCellIndexUnpacked = function(geneIds, cellIndexStart, cellIndexEnd,
                                                                               getCellNames, callback) {
}

DataControllerFile.prototype.getAvailableEmbeddings = function(type, callback, callbackParams) {

}

DataControllerFile.prototype.getEmbedding = function(type, embeddingType, callback) {

}

DataControllerFile.prototype.getAvailableReductionTypes = function(callback) {

}

DataControllerFile.prototype.getCellMetadata = function(callback) {

}

DataControllerFile.prototype.getGeneSetStoreByName = function(name, callback) {

}

DataControllerFile.prototype.getGeneInformationStore = function(callback) {

}

DataControllerFile.prototype.getGeneSetInformationStore = function(callback) {

}

DataControllerFile.prototype.getAspectMatrixByAspect = function(cellIndexStart, cellIndexEnd, aspectIds, callback) {

}
