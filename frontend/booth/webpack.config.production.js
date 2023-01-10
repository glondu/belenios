const path = require('path');

module.exports = {
    mode: 'production',
    entry: './app.mjs',
    output: {
        path: path.resolve(__dirname, "dist"),
        filename: 'bundle.production.js',
    },
};
