module.exports = {
  output: 'translations/$LOCALE.json',
  input: [
    'booth/components/*.mjs',
    'booth/app.mjs',
    'i18next-parser-explicit-declarations.js'
  ],
  locales: ['en', 'fr'],
  keepRemoved: false
}