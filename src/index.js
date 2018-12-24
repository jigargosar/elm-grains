import './main.css'
// noinspection ES6CheckImport
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import { partial, tap } from 'ramda'
import createHistory from 'history/createBrowserHistory'
import { sendToElmApp, setElmAppPortSubscriptions } from './elm-app'
import { jsonCacheGetOr, jsonCacheSet } from './json-cache'
import getFireSubscriptions from './fire'

const history = createHistory()

export const tapLog = m => tap(partial(console.log, [m]))

// noinspection JSUnresolvedVariable
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    windowSize: { width: window.innerWidth, height: window.innerHeight },
    grains: jsonCacheGetOr([], 'grains'),
    grainCache: jsonCacheGetOr([], 'grainCache'),
    url: document.URL,
  },
})

// noinspection JSUnresolvedFunction
history.listen((location, action) => {
  console.debug(action, location.pathname, location.state)
  sendToElmApp(app, 'urlChanged', document.URL)
})

window.addEventListener('keydown', () => {
  if (document.activeElement === body) {
    sendToElmApp(app, 'keyDownOnBody', null)
  }
})

setElmAppPortSubscriptions(
  {
    pushUrl: pathname => {
      // Use push, replace, and go to navigate around.
      if (history.location.pathname !== pathname) {
        history.push(pathname, { some: 'state' })
      }
    },

    error: data => {
      console.error(data)
    },
    // cacheGrains: data => {
    //   jsonCacheSet('grains', data)
    // },
    setGrainCache: data => {
      jsonCacheSet('grainCache', data)
    },
    navigateBack: () => history.goBack(),
    ...getFireSubscriptions(app),
  },
  app,
)

registerServiceWorker()
