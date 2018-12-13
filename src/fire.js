import firebase from 'firebase/app'

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

function getSubscriptions(app) {
  return {}
}

export default getSubscriptions
