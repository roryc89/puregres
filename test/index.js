const fs = require('fs')
const {promisify} = require('util')
const {Client} = require('pg')
const test = require('tape')
const testTeardown = require('./test_teardown')
const puregres = require('../lib')
const setupQuery = fs.readFileSync('test/test_setup.sql').toString()
const readFile = promisify(fs.readFile)

const dbOptions = {
  database: 'test',
  user: 'rorycampbell',
  password: '',
  host: 'localhost',
  port: 5432
}

const client = new Client(dbOptions)

test('module should read sql files and generate purescript files that call the queries', async function (t) {
  try {
    await setup()
    await runTests(t)
    t.end()
  } catch (error) {
    t.notOk(error)
    t.end()
  }
})

async function setup () {
  try {
    await client.connect()
    await testTeardown(client)
    await client.query(setupQuery)
    await client.end()
  } catch (error) {
    console.log('error', error)
    client.end()
  }
}

async function runTests (t) {
  const options = {
    ...dbOptions,
    schema: 'public',
    pattern: 'test/Queries/**/*.sql'
  }

  await puregres(options)
  const result = await readFile('test/Queries/Users/SelectUsersWhereId.generated.purs')
  const expected = await readFile('test/Queries/Users/ExpectedSelectUsersWhereId.purs')

  t.equal(
    result.toString(),
    expected.toString(),
    'SelectUsersWhereId.sql should be correctly converted to purescript'
   )

  console.log('running tests')
}
