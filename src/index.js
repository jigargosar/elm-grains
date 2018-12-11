import './main.css'
// noinspection ES6CheckImport
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import { curry, forEachObjIndexed, isNil, partial, pathOr, tap } from 'ramda'
import createHistory from 'history/createBrowserHistory'

const history = createHistory()

export const tapLog = m => tap(partial(console.log, [m]))

const sendTo = curry(function sendTo(app, port, data) {
  if (!pathOr(null, ['ports', port, 'send'])(app)) {
    console.error('sendTo port not found', port, 'data ignored', data)
    return
  }
  app.ports[port].send(data)
})

function subscribe(options, app) {
  if (!app || !app.ports) {
    console.error('no ports found', app)
    return
  }

  forEachObjIndexed((fn, sub) => {
    if (!pathOr(null, ['ports', sub, 'subscribe'])(app)) {
      console.error('sub port not found', sub)
      return
    }
    // noinspection JSIgnoredPromiseFromCall, JSCheckFunctionSignatures
    app.ports[sub].subscribe(data => fn(data, sendTo(app)))
  })(options)
}

function storageGetOr(defaultValue, key) {
  try {
    let item = localStorage.getItem(key)
    if (isNil(item)) return defaultValue
    return JSON.parse(item)
  } catch (e) {
    return defaultValue
  }
}

function storageSet(key, value) {
  if (isNil(value) || isNil(key)) {
    console.warn('Invalid Args', 'storageSet', key, value)
    return
  }
  localStorage.setItem(key, JSON.stringify(value))
}

// noinspection JSUnresolvedVariable
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    windowSize: { width: window.innerWidth, height: window.innerHeight },
    grains: storageGetOr({ items: [] }, 'grains'),
    url: document.URL,
  },
})

// noinspection JSUnresolvedFunction
history.listen((location, action) => {
  console.debug(action, location.pathname, location.state)
  sendTo(app, 'urlChanged', document.URL)
})

subscribe(
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
    cacheGrains: todos => {
      storageSet('grains', todos)
    },
  },
  app,
)

registerServiceWorker()
