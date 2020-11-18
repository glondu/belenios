module.exports = {
  keepRemoved: true,
  output: 'translations/$LOCALE.json',
  input: [
    'booth/components/*.mjs',
    'booth/app.mjs'
  ],
  locales: ['en', 'fr']
}