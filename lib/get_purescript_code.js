const {parse} = require('pg-query-parser')
const R = require('ramda')
// converts sql code into a purescript function
// getPurescriptCode :: Map String Table -> String -> String
const getPurescriptCode = (tables, sqlCode) => {
  const {query, error} = parse(sqlCode)

  if (error) {
    throw new Error(error)
  }

  const {targetList, fromClause, whereClause} = query[0].SelectStmt

  console.log('targetList', JSON.stringify(targetList, null, 4))
  console.log('fromClause', JSON.stringify(fromClause, null, 4))
  console.log('whereClause', JSON.stringify(whereClause, null, 4))

  const args = whereClause.BoolExpr.args
    .filter(hasAParam)
    .map(getArgNameAndType(tables, fromClause))

  console.log('tables', Array.from(tables.values()))
  console.log('users', Array.from(tables.get('users').columns.values()))

  return sqlCode
}

const hasAParam = (whereArg) =>
  Boolean(whereArg.A_Expr.rexpr.ParamRef)

const getArgNameAndType = R.curry((tables, fromClause, whereArg) => ({
  name: getArgName(whereArg),
  type: getArgType(tables, fromClause, whereArg)
}))

const getArgName = (whereArg) =>
  R.last(whereArg.A_Expr.lexpr.ColumnRef.fields).String.str

const getArgType = (tables, fromClause, whereArg) => {
  const {fields} = whereArg.A_Expr.lexpr.ColumnRef
  R.cond([
    [1, () => tables.get(fields[0]).get(fields[1])],
    [2, () => tables.get(fields[0]).get(fields[1])],
    [3, () => tables.get(fields[1]).get(fields[2])]
  ])(fields.length)
}
module.exports = R.curry(getPurescriptCode)
