const translationsBackendOptions = { // these are options to i18nextHttpBackend plugin of i18next
  loadPath: '/static/locales/frontend/{{lng}}.json'
};

i18next
  .use(window.i18nextHttpBackend)
  .use(ReactI18next.initReactI18next) // passes i18n down to react-i18next
  .init({
    lng: "en",
    debug: true,
    fallbackLng: "en",
    backend: translationsBackendOptions,
    interpolation: {
      escapeValue: false // React already safes from XSS
    }
  }, function(err, t) {
    // init set content
    updateContent();
  });

function updateContent() {
  document.getElementById('i18n-output').innerHTML = i18next.t("Welcome to React");
}

i18next.on('languageChanged', () => {
  updateContent();
});
