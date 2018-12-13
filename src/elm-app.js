import { curry, forEachObjIndexed, pathOr } from 'ramda'

export const sendToElmApp = curry(function sendTo(app, port, data) {
  if (!pathOr(null, ['ports', port, 'send'])(app)) {
    console.error('sendToElmApp port not found', port, 'data ignored', data)
    return
  }
  app.ports[port].send(data)
})

export function setElmAppPortSubscriptions(options, app) {
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
    app.ports[sub].subscribe(data => fn(data, sendToElmApp(app)))
  })(options)
}
