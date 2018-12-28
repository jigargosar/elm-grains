// noinspection ES6CheckImport
import { Elm } from './Worker.elm'
import { setElmAppPortSubscriptions } from './elm-app'
import { jsonCacheGetOr } from './json-cache'

// noinspection JSUnresolvedVariable
const app = Elm.Worker.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    windowSize: { width: window.innerWidth, height: window.innerHeight },
    grainCache: jsonCacheGetOr([], 'grainCache'),
    url: document.URL,
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
