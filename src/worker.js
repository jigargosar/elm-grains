// noinspection ES6CheckImport
import { Elm } from './Worker.elm'
import { setElmAppPortSubscriptions } from './elm-app'

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
