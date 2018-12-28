// noinspection ES6CheckImport
import { Elm } from './Worker.elm'
import { setElmAppPortSubscriptions } from './elm-app'
import { jsonCacheGetOr } from './json-cache'
import * as R from 'ramda'

export const tapLog = m => R.tap(R.partial(console.log, [m]))

// noinspection JSUnresolvedVariable
const app = Elm.Worker.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    windowSize: { width: window.innerWidth, height: window.innerHeight },
    grains: jsonCacheGetOr([], 'grains'),
    grainCache: jsonCacheGetOr([], 'grainCache'),
    url: document.URL,
  },
})

setElmAppPortSubscriptions(
  {
    error: data => {
      console.error(data)
    },
    // cacheGrains: data => {
    //   jsonCacheSet('grains', data)
    // },
  },
  app,
)
