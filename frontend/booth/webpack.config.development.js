const path = require('path');

module.exports = {
    mode: 'development',
    entry: './app.mjs',
    output: {
        path: path.resolve(__dirname, "dist"),
        filename: 'bundle.development.js',
    },
};
