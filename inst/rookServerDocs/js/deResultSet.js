"use strict";

/**
  * Represents a differential expression result set and accompanying metadata
* @constructor
*/
  function deResultSet() {
    this.selectionA = null;
    this.selectionB = null;
    this.results = null;
    this.name = null;
    this.startTime = null;
    this.endTime = null;
  };


/**
  * get the name
*/
  deResultSet.prototype.getName = function() {
    return this.name;
  };
  
  /**
    * set the name
  */
    deResultSet.prototype.setName = function(val) {
      this.name = val;
    };
    
    /**
      * get the startTime
    */
      deResultSet.prototype.getStartTime = function() {
        return this.startTime;
      };
      
      /**
        * set the startTime
      */
        deResultSet.prototype.setStartTime = function(val) {
          this.startTime = val;
        };
        
        /**
          * get the endTime
        */
          deResultSet.prototype.getEndTime = function() {
            return this.endTime;
          };
          
          /**
            * set the endTime
          */
            deResultSet.prototype.setEndTime = function(val) {
              this.endTime = val;
            };
            
            
            /**
              * Get the first cell selection (an array of cell ids)
            */
              deResultSet.prototype.getSelectionA = function() {
                return this.selectionA;
              };
              
              /**
                * Set the first cell selection (an array of cell ids)
              */
                deResultSet.prototype.setSelectionA = function(val) {
                  this.selectionA = val;
                };
                
                /**
                  * Set the second cell selection (an array of cell ids)
                */
                  deResultSet.prototype.getSelectionB = function() {
                    return this.selectionB;
                  };
                  
                  
                  /**
                    * Get the second cell selection (an array of cell ids)
                  */
                    deResultSet.prototype.setSelectionB = function(val) {
                      this.selectionB = val;
                    };
                    
                    
                    /**
                      * Get the table of results
                    */
                      deResultSet.prototype.getResults = function() {
                        return this.results;
                      };
                      
                      
                      /**
                        * Set the table of results
                      */
                        deResultSet.prototype.setResults = function(val) {
                          this.results = val;
                        };
                        
                        