import * as R from 'ramda'

export const tapLog = m => R.tap(R.partial(console.log, [m]))
