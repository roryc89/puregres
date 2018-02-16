require('dotenv').config()
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
  user: process.env.DB_USER,
  password: process.env.DB_PASSWORD || '',
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
    pattern: 'test/Query/**/*.sql',
    pureRoot: 'Query' // which file should the top module ancestor
  }

  await puregres(options)

  let result
  let expected

  result = await readFile('test/Query/Select/SelectUsersWhereId.generated.purs')
  expected = await readFile('test/Query/Select/ExpectedSelectUsersWhereId.purs')

  t.equal(
    result.toString(),
    expected.toString(),
    'SelectUsersWhereId.sql should be correctly converted to purescript' +
    ' handling a mix of full and partial column names.'
   )

  result = await readFile('test/Query/Select/SelectUsersAndOrders.generated.purs')
  expected = await readFile('test/Query/Select/ExpectedSelectUsersAndOrders.purs')

  t.equal(
     result.toString(),
     expected.toString(),
     'SelectUsersAndOrders.sql should be correctly converted to purescript, ' +
     ' handling a join.'
  )

  result = await readFile('test/Query/Select/SelectWith2Joins.generated.purs')
  expected = await readFile('test/Query/Select/ExpectedSelectWith2Joins.purs')

  t.equal(
     result.toString(),
     expected.toString(),
     'SelectWith2Joins.sql should be correctly converted to purescript, ' +
     ' handling 2 joins.'
    )

  result = await readFile('test/Query/Select/SelectWithSubQuery.generated.purs')
  expected = await readFile('test/Query/Select/ExpectedSelectWithSubQuery.purs')

  t.equal(
     result.toString(),
     expected.toString(),
     'SelectWithSubQuery.sql should be correctly converted to purescript, ' +
     ' handling a sub query.'
    )

  result = await readFile('test/Query/Select/SelectWithLeftJoin.generated.purs')
  expected = await readFile('test/Query/Select/ExpectedSelectWithLeftJoin.purs')

  t.equal(
     result.toString(),
     expected.toString(),
     'SelectWithLeftJoin.sql should be correctly converted to purescript, ' +
     ' handling a left join.'
    )

  result = await readFile('test/Query/Select/SelectWithRightJoin.generated.purs')
  expected = await readFile('test/Query/Select/ExpectedSelectWithRightJoin.purs')

  t.equal(
     result.toString(),
     expected.toString(),
     'SelectWithRightJoin.sql should be correctly converted to purescript, ' +
     ' handling a right join.'
    )

  result = await readFile('test/Query/Select/SelectWithOuterJoin.generated.purs')
  expected = await readFile('test/Query/Select/ExpectedSelectWithOuterJoin.purs')

  t.equal(
     result.toString(),
     expected.toString(),
     'SelectWithOuterJoin.sql should be correctly converted to purescript, ' +
     ' handling an outer join.'
    )

  result = await readFile('test/Query/Select/SelectWithNoWhere.generated.purs')
  expected = await readFile('test/Query/Select/ExpectedSelectWithNoWhere.purs')

  t.equal(
     result.toString(),
     expected.toString(),
     'SelectWithNoWhere.sql should be correctly converted to purescript, ' +
     ' handling a query with no where clause.'
    )
}
