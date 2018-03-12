exports.unsafeToStringJs = function(value) {
  if (value === null) {
    return "null";
  }
  if (value === undefined) {
    return "undefined";
  }
  return value.toString(); // unsafe and should only be used for testing purposes
};
