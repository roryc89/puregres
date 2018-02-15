const assignResults = (result, isMaybe, tableExpression) => {
  if (tableExpression.RangeVar) {
    result[tableExpression.RangeVar.relname] = isMaybe
  }
  if (tableExpression.JoinExpr) {
    const joinType = joinTypes[tableExpression.JoinExpr.jointype]
    isMaybe = joinType === 'OUTER' || isMaybe
    assignResults(result, isMaybe || joinType === 'RIGHT', tableExpression.JoinExpr.larg)
    assignResults(result, isMaybe || joinType === 'LEFT', tableExpression.JoinExpr.rarg)
  }
  return []
}

// checks if all return values in a table should be maybe due to left/right joinTypes
// getTableNamesToMaybes :: FromClause -> StrMap Boolean
const getTableNamesToMaybes = (fromClause) => {
  const result = {}
  assignResults(result, false, fromClause[0])
  return result
}
const joinTypes = {
  '0': 'INNER',
  '1': 'LEFT',
  '2': 'OUTER',
  '3': 'RIGHT'
}

module.exports = getTableNamesToMaybes
