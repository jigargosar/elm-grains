import * as plugin from 'prettier-plugin-elm'
import * as R from 'ramda'

export const defaultOptions = plugin.defaultOptions
export const languages = plugin.languages
export const parsers = R.mergeDeepLeft(
  {
    elm: {
      parse: function(...args) {
        const result = plugin.parsers.elm.parse(...args)
        console.log('result', result)
        // return { ...result }
      },
    },
  },
  plugin.parsers,
)
// console.log(parsers)
export const printers = plugin.printers
