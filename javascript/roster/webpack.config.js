var path = require("path");
var webpack = require('webpack');

var isProductionMode = process.env.NODE_ENV === 'production';

var config = {
  debug: true,
  entry: {
    "roster": "./roster.js"
  },
  output: {
    path: path.join(__dirname, "public"),
    filename: 'roster.js'
  },
  module: {
    loaders: [ 
      { test: /strophe/, loader: 'exports?Strophe=window.Strophe,$pres=window.$pres,$iq=window.$iq' },
      { test: /\.jsx?$/, exclude: /node_modules/, loader: 'babel' },
      { test: /\.(png|gif|woff|woff2|eot|ttf|svg)(\?.*)?$/, loader: 'url-loader?limit=50000' },
      { test: /\.css$/, loaders: ["style-loader", "css-loader" + (isProductionMode ? "" : "?sourceMap")] }
    ]
  },
  devtool: 'cheap-source-map'
};

module.exports = config;
