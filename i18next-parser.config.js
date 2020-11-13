module.exports = {
  keepRemoved: true,
  output: 'frontend_translations/$LOCALE.json',
  input: [
    'src/booth/js/components/*.mjs',
    'src/booth/js/app.mjs'
  ],
  locales: ['en', 'fr']
}