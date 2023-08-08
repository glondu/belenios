module.exports = {
  i18nextOptions: { compatibilityJSON: 'v3' },
  output: 'translations/$LOCALE.json',
  input: [
    'booth/components/*.js',
    'booth/app.js'
  ],
  locales: ['en', 'ar', 'cs', 'de', 'el', 'es', 'es_419', 'fi', 'fr', 'it', 'jpn_JP', 'lt', 'nb', 'oc', 'pl', 'pt_BR', 'ro', 'uk'],
  keepRemoved: false,
  indentation: 4
}
