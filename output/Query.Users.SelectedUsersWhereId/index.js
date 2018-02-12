"use strict";
var query = "SELECT\x0a  user_id,\x0a  email,\x0a  last_name\x0aFROM users\x0aWHERE user_id = $1\x0aAND registered = TRUE";
module.exports = {
    query: query
};
