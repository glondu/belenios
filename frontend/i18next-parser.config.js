module.exports = {
  output: 'translations/$LOCALE.json',
  input: [
    'booth/components/*.js',
    'booth/app.js',
    'i18next-parser-explicit-declarations.js'
  ],
  locales: ['en', 'fr'],
  keepRemoved: false,
  indentation: 4
}