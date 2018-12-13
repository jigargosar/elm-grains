import firebase from 'firebase/app'
import 'firebase/auth'
import 'firebase/firestore'
import { sendToElmApp } from './elm-app'
import * as R from 'ramda'

const disposers = []
function addDisposer(disposer) {
  disposers.push(disposer)
}
function disposeAll() {
  disposers.forEach(d => d())
  disposers.splice(0, disposers.length)
}

// if(module.hot){
//   module.hot.accept()
//   module.hot.dispose(()=>{
//     disposeAll()
//   })
// }

function init() {
  const config = {
    apiKey: 'AIzaSyBVS1Tx23pScQz9w4ZDTGh307mqkCRy2Bw',
    authDomain: 'not-now-142808.firebaseapp.com',
    databaseURL: 'https://not-now-142808.firebaseio.com',
    projectId: 'not-now-142808',
    storageBucket: 'not-now-142808.appspot.com',
    messagingSenderId: '476064436883',
  }
  firebase.initializeApp(config)

  const auth = firebase.auth()
  const firestore = firebase.firestore()

  firestore.settings({ timestampsInSnapshots: true })
  return { auth, firestore }
}

const { auth, firestore } = init()

const createCRef = cName =>
  firestore.collection(`users/${auth.currentUser.uid}/${cName}`)

function getFireSubscriptions(app) {
  const send = sendToElmApp(app, 'fire2Elm')
  auth.onAuthStateChanged(function(user) {
    if (user) {
      send({
        msg: 'UserLoggedIn',
        payload: { user: R.pick(['displayName', 'uid', 'email'])(user) },
      })
    } else {
      send({ msg: 'UserNotLoggedIn', payload: {} })
    }
  })
  return {
    signIn: async () => {
      const gp = new firebase.auth.GoogleAuthProvider()
      gp.setCustomParameters({ prompt: 'select_account' })
      await auth.signInWithPopup(gp)
    },
    signOut: () => auth.signOut(),
    persistGrains: ({ list }) => {
      console.log(`list`, list)
      const gcRef = createCRef('grains')
      list.forEach(g => gcRef.doc(g.id).set(g))
    },
  }
}

export default getFireSubscriptions
