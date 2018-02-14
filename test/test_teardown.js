const fs = require('fs')
const del = require('del')
const testTeardownQuery = fs.readFileSync('test/test_teardown.sql').toString()

module.exports = async function (client) {
  await Promise.all([
    del(['**/*.generated.purs']),
    client.query(testTeardownQuery)
  ])
}
