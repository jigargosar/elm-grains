const path = require('path')

module.exports = {
  entry: 'src/worker.js',
  output: {
    path: 'dist',
    filename: 'bundle.js',
  },
}
