function i18n_init(language_code="en", onInitialized=null, onLanguageChanged=null){
  const translationsBackendOptions = { // these are options to i18nextHttpBackend plugin of i18next
    loadPath: '../translations/{{lng}}.json'
  };

  i18next
    .use(window.i18nextHttpBackend)
    .use(ReactI18next.initReactI18next) // passes i18n down to react-i18next
    .init(
      {
        lng: language_code,
        debug: true,
        supportedLngs: ["en", "fr", "pl", "nb"],
        fallbackLng: { // In order to not download the fallback language translation file when it won't be needed, languages in which the application is fully translated should have an empty array for their key here
          "fr": [],
          "en": [],
          "default": ["en"]
        },
        load: "currentOnly",
        backend: translationsBackendOptions,
        interpolation: {
          escapeValue: false // React already safes from XSS
        }
      },
      onInitialized
    );

  if(onLanguageChanged){
    i18next.on('languageChanged', onLanguageChanged);
  }
}

export { i18n_init };
export default i18n_init;
