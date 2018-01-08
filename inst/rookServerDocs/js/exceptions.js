"use strict";

const STATIC_FILE_FIELD_MISSING=0x001;
const FUNCTIONALITY_NOT_SUPPORTED=0x002;

/**
 * Standard pagoda2 runtime exception
 * @param code numeric code for exception
 * @param message message for exception
 * @constructor
 */
function RuntimeException(code, message) {
  this.code = code;
  this.message = message;
}

