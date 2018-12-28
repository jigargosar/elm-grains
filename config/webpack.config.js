const path = require('path')

module.exports = {
  entry: './src/worker.js',
  output: {
    path: path.resolve(__dirname, '../dist'),
    filename: 'bundle.js',
  },
  module: {
    rules: [
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]',
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        // This is what you need in your own work
        loader: 'elm-webpack-loader',
        // loader: '../index.js',
        options: {
          debug: true,
        },
      },
    ],
  },

  devServer: {
    inline: true,
    stats: 'errors-only',
  },
}
