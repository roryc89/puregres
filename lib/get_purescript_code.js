const {parse} = require('pg-query-parser')
const fs = require('fs')
const R = require('ramda')
const Handlebars = require('handlebars')
const camelCase = require('camelcase')
const pascalCase = require('pascal-case')
const purescriptCode = fs.readFileSync('lib/purescript_code.hbs').toString()
const template = Handlebars.compile(purescriptCode)
// converts sql code into a purescript function
// getPurescriptCode :: Map String Table -> String -> String
const getPurescriptCode = (filename, tables, sqlCode) => {
  const {query, error} = parse(sqlCode)

  if (error) {
    throw new Error(error)
  }

  const {targetList, fromClause, whereClause} = query[0].SelectStmt

  const args = whereClause.BoolExpr.args
    .filter(hasAParam)
    .map(getWhereArgNameAndType(tables, fromClause))

  const returns = targetList
    .map(getTargetNameAndType(tables, fromClause))

  const argTypes = R.pipe(
    R.pluck('type'),
    R.join('\n  -> ')
  )(args)

  const argNames = R.pipe(
    R.pluck('name'),
    R.join(' ')
  )(args)

  const toSqls = R.pipe(
    R.map((arg) => `toSql ${arg.name}`),
    R.join(', ')
  )(args)

  const returnRecordBody = R.pipe(
    R.map((r) => `${r.name} :: ${r.type}`),
    R.join('\n  , ')
  )(returns)

  const decodeLogic = R.pipe(
    R.map((r) => r.maybe
      ? `${r.name}: remove $ f ! "${r.name}" >>= readNull >>= traverse decode`
      : `${r.name}: remove $ f ! "${r.name}" >>= decode`
    ),
    R.join('\n      , ')
  )(returns)

  const returnTypeName = filename + 'Row'

  const functionName = camelCase(filename)

  return template({
    argTypes,
    argNames,
    toSqls,
    returnRecordBody,
    decodeLogic,
    returnTypeName,
    functionName,
    query: sqlCode
  })
}

const hasAParam = (whereArg) =>
  Boolean(whereArg.A_Expr.rexpr.ParamRef)

const getWhereArgNameAndType = R.curry((tables, fromClause, whereArg) => {
  const {maybe, type} = getArgType(tables, fromClause, whereArg.A_Expr.lexpr.ColumnRef.fields)
  return {
    maybe,
    type,
    name: getName(whereArg.A_Expr.lexpr.ColumnRef.fields)
  }
})

const getTargetNameAndType = R.curry((tables, fromClause, target) => {
  const {maybe, type} = getArgType(tables, fromClause, target.ResTarget.val.ColumnRef.fields)
  return {
    maybe,
    type,
    name: getName(target.ResTarget.val.ColumnRef.fields)
  }
})

const getName = (fields) =>
  R.last(fields).String.str

const getArgType = (tables, fromClause, fields) => {
  const [table, columnName] = R.cond([
    [R.equals(1), () => [findTable(tables, fromClause, fields[0].String.str), fields[0].String.str]],
    [R.equals(2), () => [fields[0].String.str, fields[1].String.str]],
    [R.equals(3), () => [fields[1].String.str, fields[2].String.str]],
    [R.T, () => { throw new Error('invalid number of fields: ' + fields.length) }]
  ])(fields.length)

  const column = tables.get(table).get(columnName)

  const maybe = column.allowNull

  const type = maybe
    ? `Maybe (${postgresToPureTypes[column.type]})`
    : postgresToPureTypes[column.type]

  return {
    type,
    maybe
  }
}

const findTable = (tables, fromClause, column) => {
  const tableNames = fromClause
    .map((table) => table.RangeVar.relname)
    .filter((tableName) =>
      tables.get(tableName) && tables.get(tableName).get(column)
    )
  if (tableNames.length > 1) {
    throw new Error(`Multiple tables with column. Column: ${column} . Tables: ${tableNames}`)
  }
  if (tableNames.length === 0) {
    throw new Error('No tables with column found: ' + column)
  }
  return tableNames[0]
}

module.exports = R.curry(getPurescriptCode)

const postgresToPureTypes = {
  'integer': 'Int',
  'character varying': 'String',
  'text': 'String',
  'boolean': 'Boolean',
  'jsonb': 'Foreign', // try to avoid
  'json': 'Foreign', // really try to avoid
  'bigint': 'Number',
  'smallint': 'Int',
  'numeric': 'Number',
  'date': 'String',
  'user-defined': 'String' // should be handled better by ok for our purposes
}
