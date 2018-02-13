const fs = require('fs')
const {promisify} = require('util')
const path = require('path')
const pgStructure = require('pg-structure')
const getPurescriptCode = require('./get_purescript_code')
const glob = promisify(require('glob'))
const readFile = promisify(fs.readFile)
const writeFile = promisify(fs.writeFile)

//
module.exports = async function (options) {
  try {
    const {database, user, password, host, port, schema, pattern} = options

    const pgStructureOptions = {database, user, password, host, port}

    const [db, filenames] = [await pgStructure(pgStructureOptions, [schema]), await glob(pattern)]

    const files = await Promise.all(filenames.map((f) => readFile(f)))
    const {tables} = db.schemas.get(schema)

    const purescriptFiles = files
      .map((buffer) => buffer.toString())
      .map((file, i) =>
        getPurescriptCode(path.parse(filenames[i]).name, tables, file)
      )

    await Promise.all(
      purescriptFiles
        .map((file, i) =>
          writeFile(getPursName(filenames[i]), file)
        )
    )
  } catch (error) {
    console.log(error)
  }
}

const getPursName = (filename) => {
  const parsed = path.parse(filename)
  return `${parsed.dir}/${parsed.name}.generated.purs`
}
