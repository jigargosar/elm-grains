import { Elm } from './Worker.elm'
import { setElmAppPortSubscriptions } from './elm-app'
import { jsonCacheGetOr, jsonCacheSet } from './json-cache'

export const tapLog = m => tap(partial(console.log, [m]))

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
    setGrainCache: data => {
      jsonCacheSet('grainCache', data)
    },
  },
  app,
)

registerServiceWorker()
