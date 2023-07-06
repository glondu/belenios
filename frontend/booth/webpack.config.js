const path = require("path");

module.exports = {
  entry: "./app.mjs",
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "bundle.js",
  },
};
