import './main.css'
// noinspection ES6CheckImport
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import { partial, tap } from 'ramda'
import createHistory from 'history/createBrowserHistory'
import { sendToElmApp, setElmAppPortSubscriptions } from './elm-app'
import { jsonCacheGetOr, jsonCacheSet } from './json-cache'
import getFireSubscriptions from './fire'
import autoSize from 'autosize'

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
  // console.log(`location`, location)
  // console.log(`history`, history)
  const urlChangedEvent = {
    url: document.URL,
    action,
    state: location.state,
  }
  console.log(`urlChangedEvent`, urlChangedEvent)
  sendToElmApp(app, 'urlChanged', urlChangedEvent)
})

window.addEventListener('keydown', event => {
  if (document.activeElement === document.body) {
    sendToElmApp(app, 'keyDownOnBody', event)
  }
})
window.autoSize = autoSize
autoSize(document.querySelectorAll('textarea'))

setElmAppPortSubscriptions(
  {
    pushUrl: pathname => {
      // Use push, replace, and go to navigate around.
      if (history.location.pathname !== pathname) {
        history.push(pathname)
      }
    },
    replaceState: state => {
      const newLocationWithState = Object.assign({}, history.location, {
        state,
      })
      history.replace(newLocationWithState)
    },
    autoSize: domId => {
      requestAnimationFrame(() => {
        autoSize(document.getElementById(domId))
      })
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
