import firebase from 'firebase/app'
import 'firebase/auth'
import 'firebase/firestore'
import { sendToElmApp } from './elm-app'
import * as R from 'ramda'
import debounce from 'lodash.debounce'

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

function persistSavedGrainList(savedGrains) {
  console.log(`fire: debounced: persistSavedGrainList started`, savedGrains)
  const gcRef = createCRef('grains')
  // https://github.com/topics/diff?l=javascript&q=json&unscoped_q=json
  // https://github.com/benjamine/jsondiffpatch
  // https://github.com/mattphillips/deep-object-diff#detaileddiff
  // https://github.com/RobinBressan/json-git
  // https://github.com/flitbit/diff
  // https://github.com/cosmicanant/recursive-diff
  const batch = firestore.batch()
  savedGrains.forEach(({ initial, latest }) =>
    batch.set(gcRef.doc(latest.id), latest),
  )
  batch.commit()
  console.log(`fire: debounced: persistSavedGrainList completed`)
}

const debouncedPersistSavedGrainList = debounce(persistSavedGrainList, 1000, {
  leading: false,
  trailing: true,
})

function getFireSubscriptions(app) {
  const send = sendToElmApp(app, 'fire2Elm')
  let grainsListener = R.identity
  auth.onAuthStateChanged(function(user) {
    if (user) {
      send({
        msg: 'UserLoggedIn',
        payload: { user: R.pick(['displayName', 'uid', 'email'])(user) },
      })
      grainsListener()
      grainsListener = createCRef('grains').onSnapshot(function(qs) {
        const changes = qs
          .docChanges()
          .map(({ type, doc }) => ({ type, doc: doc.data() }))
        console.log('Change Snapshot Received', changes)
        send({ msg: 'GrainChanges', payload: { changes } })
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
    persistSavedGrainList: async savedGrains => {
      if (!auth.currentUser) {
        console.warn(
          'persistSavedGrainList: User Not SignedIn. Ignoring',
          savedGrains,
        )
        return
      }
      console.log(`debouncing: fire: persistSavedGrainList`, savedGrains)
      debouncedPersistSavedGrainList(savedGrains)
    },
    // persistSavedGrain: async savedGrain => {
    //   console.log(`fire: persistSavedGrain started`, savedGrain)
    //   const gcRef = createCRef('grains')
    //   const { initial, latest } = savedGrain
    //   // https://github.com/topics/diff?l=javascript&q=json&unscoped_q=json
    //   // https://github.com/benjamine/jsondiffpatch
    //   // https://github.com/mattphillips/deep-object-diff#detaileddiff
    //   // https://github.com/RobinBressan/json-git
    //   // https://github.com/flitbit/diff
    //   // https://github.com/cosmicanant/recursive-diff
    //   gcRef.doc(latest.id).set(latest)
    //   console.log(`fire: persistSavedGrain completed`)
    // },
    // persistNewGrain: async grain => {
    //   console.log(`fire: persistNewGrain started`, grain)
    //   const gcRef = createCRef('grains')
    //   gcRef.doc(grain.id).set(grain)
    //   console.log(`fire: persistNewGrain completed`)
    // },
    // persistUpdatedGrain: async grain => {
    //   console.log(`fire: persistUpdatedGrain started`, grain)
    //   const gcRef = createCRef('grains')
    //   gcRef.doc(grain.id).set(grain)
    //   console.log(`fire: persistUpdatedGrain completed`)
    // },
    // persistRemovedGrain: async grain => {
    //   console.log(`fire: persistRemovedGrain started`, grain)
    //   const gcRef = createCRef('grains')
    //   gcRef.doc(grain.id).delete()
    //   console.log(`fire: persistRemovedGrain completed`)
    // },
  }
}

export default getFireSubscriptions
