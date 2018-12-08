import './main.css'
// noinspection ES6CheckImport
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import { curry, forEachObjIndexed, isNil, partial, pathOr, tap } from 'ramda'

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
    grains: localStorage.getItem("grains") || "[]",
    labels: localStorage.getItem("labels") || "[]",
    userLabels: localStorage.getItem("userLabels") || "[]",
  },
})

subscribe(
  {
    // warn: data => {
    //   console.warn(data)
    // },
    // focusId: id => {
    //   const el = document.getElementById(id)
    //   if (el) {
    //     el.focus()
    //   } else {
    //     console.error('Focus: El Not Found', id)
    //   }
    // },
    //
    // focusSelector: selector => {
    //   let escapedSelector = selector.replace('~', '\\~')
    //   const el = document.querySelector(escapedSelector)
    //   if (el) {
    //     el.focus()
    //   } else {
    //     console.error('Focus: Selector Not Found', escapedSelector)
    //   }
    // },

    error: data => {
      console.error(data)
    },
    cacheGrainList: todos => {
      storageSet('grains', todos)
    },
    // cacheContextStore: contexts => {
    //   storageSet('contexts', contexts)
    // },
  },
  app,
)

registerServiceWorker()
