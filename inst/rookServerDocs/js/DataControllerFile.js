
/**
 * Handles data access by File, either local or remote
 */
function DataControllerFile(loadParams) {
  if(loadParams.connectionType != 'remoteFile') {
    throw new Error('DataControllerFile does not support the selected connenction type');
  }

  // Make a p2file reader with the appropriate settings and save it here


}

DataControllerFile.prototype.getReducedDendrogram = function(callback) {

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
