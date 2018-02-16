const {parse} = require('pg-query-parser')
const fs = require('fs')
const R = require('ramda')
const Handlebars = require('handlebars')
const camelCase = require('camelcase')
const path = require('path')
const purescriptCode = fs.readFileSync(path.join(__dirname, 'purescript_code.hbs')).toString()
const template = Handlebars.compile(purescriptCode)
const getTableNamesToMaybes = require('./get_table_names_to_maybes')
// converts sql code into a purescript function
// getPurescriptCode :: Map String Table -> String -> String
const getPurescriptCode = (pureRoot, filepath, tables, sqlCode) => {
  const {query, error} = parse(sqlCode)

  filepath = R.dropWhile((p) => p !== pureRoot, filepath.split('/'))

  const filename = R.last(filepath)

  if (error) {
    throw new Error(error)
  }

  const {targetList, fromClause, whereClause} = query[0].SelectStmt

  const tableNamesToMaybes = getTableNamesToMaybes(fromClause)

  const getRegularArg = (fromClause_) => R.pipe(
    R.filter(hasAParam),
    R.map(getWhereArgNameAndType(tables, fromClause_))
  )

  const getArgs = (fromClause_) => R.pipe(
    (whereClause_) => {
      return whereClause_.A_Expr
      ? [whereClause_]
      : whereClause_.BoolExpr.args
    },
    R.chain((arg) => arg.A_Expr.rexpr && arg.A_Expr.rexpr.SubLink
      // recurse if subquery
      ? getArgs(arg.A_Expr.rexpr.SubLink.subselect.SelectStmt.fromClause)(arg.A_Expr.rexpr.SubLink.subselect.SelectStmt.whereClause)
      : getRegularArg(fromClause_)([arg])
    ),
    R.sortBy(R.prop('number'))
  )

  const args = whereClause ? getArgs(fromClause)(whereClause) : []

  const returns = targetList
    .map(getTargetNameAndType(tableNamesToMaybes, tables, fromClause))

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
      ? `${r.name}: unsafeRemoveFromFail $ f ! "${r.name}" >>= readNull >>= traverse decode`
      : `${r.name}: unsafeRemoveFromFail $ f ! "${r.name}" >>= decode`
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
    moduleName: filename,
    moduleNameAndPath: filepath.join('.'),
    query: sqlCode,
    hasArgs: args.length > 0
  })
}

const hasAParam = (whereArg) =>
  Boolean(whereArg.A_Expr.rexpr.ParamRef)

const getWhereArgNameAndType = R.curry((tables, fromClause, whereArg) => {
  const {maybe, typeWoMaybe} = getArgType({}, tables, fromClause, whereArg.A_Expr.lexpr.ColumnRef.fields)
  const {number} = whereArg.A_Expr.rexpr.ParamRef
  return {
    maybe,
    number,
    type: typeWoMaybe,
    name: getName(whereArg.A_Expr.lexpr.ColumnRef.fields)
  }
})

const getTargetNameAndType = R.curry((tableNamesToMaybes, tables, fromClause, target) => {
  const {maybe, type} = getArgType(tableNamesToMaybes, tables, fromClause, target.ResTarget.val.ColumnRef.fields)
  return {
    maybe,
    type,
    name: getName(target.ResTarget.val.ColumnRef.fields)
  }
})

const getName = (fields) =>
  R.last(fields).String.str

const getArgType = (tableNamesToMaybes, tables, fromClause, fields) => {
  const [table, columnName] = R.cond([
    [R.equals(1), () => [findTable(tables, fromClause, fields[0].String.str), fields[0].String.str]],
    [R.equals(2), () => [fields[0].String.str, fields[1].String.str]],
    [R.equals(3), () => [fields[1].String.str, fields[2].String.str]],
    [R.T, () => { throw new Error('invalid number of fields: ' + fields.length) }]
  ])(fields.length)

  const column = tables.get(table).get(columnName)

  const maybe = column.allowNull || tableNamesToMaybes[table]

  const type = maybe
    ? `Maybe (${postgresToPureTypes[column.type]})`
    : postgresToPureTypes[column.type]

  const typeWoMaybe = postgresToPureTypes[column.type]

  return {
    type,
    typeWoMaybe,
    maybe
  }
}

const findTable = (tables, fromClause, column) => {
  const tableNames = R.pipe(
    R.chain(getTablesFromTableExpression),
    R.filter((tableName) =>
      tables.get(tableName) && tables.get(tableName).get(column)
    )
  )(fromClause)

  if (tableNames.length > 1) {
    throw new Error(`Multiple tables with column. Column: ${column} . Tables: ${tableNames}`)
  }
  if (tableNames.length === 0) {
    throw new Error('No tables with column found: ' + column)
  }
  return tableNames[0]
}

const getTablesFromTableExpression = (tableExpression) => {
  if (tableExpression.RangeVar) {
    return [tableExpression.RangeVar.relname]
  }
  if (tableExpression.JoinExpr) {
    return [
      ...getTablesFromTableExpression(tableExpression.JoinExpr.larg),
      ...getTablesFromTableExpression(tableExpression.JoinExpr.rarg)
    ]
  }
  return []
}

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

module.exports = R.curry(getPurescriptCode)
