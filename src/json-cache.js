import { isNil } from 'ramda'

export function jsonCacheGetOr(defaultValue, key) {
  try {
    let item = localStorage.getItem(key)
    if (isNil(item)) return defaultValue
    return JSON.parse(item)
  } catch (e) {
    return defaultValue
  }
}

export function jsonCacheSet(key, value) {
  if (isNil(value) || isNil(key)) {
    console.warn('Invalid Args', 'jsonCacheSet', key, value)
    return
  }
  localStorage.setItem(key, JSON.stringify(value))
}
