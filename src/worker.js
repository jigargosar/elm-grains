// noinspection ES6CheckImport
import { Elm } from './Worker.elm'
import { setElmAppPortSubscriptions } from './elm-app'

import fs from 'fs'

const filePath = './src/Main.elm'

console.log(`filePath`, filePath)

const fileBuffer = fs.readFileSync(filePath, { encoding: 'utf8' })
console.log(`fs.readFileSync: fileContent`)
console.log(fileBuffer)
// console.log(`fs.readFileSync`, fileBuffer.toString())

// noinspection JSUnresolvedVariable
const app = Elm.Worker.init({
  flags: {
    now: Date.now(),
  },
})

setElmAppPortSubscriptions(
  {
    error: data => {
      console.error(data)
    },
  },
  app,
)
