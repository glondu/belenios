i18next
  .use(ReactI18next.initReactI18next) // passes i18n down to react-i18next
  .init({
    lng: "en",
    debug: true,
    fallbackLng: "en",
    resources: {
      en: {
        translation: {
          "Welcome to React": "Welcome to React and react-i18next",
          "Like": "Like",
          "You liked this": "You liked this."
        }
      },
      fr: {
        translation: {
          "Welcome to React": "Bienvenue à React et à react-i18next",
          "Like": "J'aime",
          "You liked this": "Vous aimez ceci."
        }
      }
    },
    interpolation: {
      escapeValue: false
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
