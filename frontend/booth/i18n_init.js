import i18next from "i18next";
import { initReactI18next } from "react-i18next";
import i18nextHttpBackend from "i18next-http-backend";

function i18n_init(
  language_code = "en",
  onInitialized = null,
  onLanguageChanged = null,
) {
  const translationsBackendOptions = {
    // these are options to i18nextHttpBackend plugin of i18next
    loadPath: "static/frontend/translations/{{lng}}.json",
  };

  i18next
    .use(i18nextHttpBackend)
    .use(initReactI18next) // passes i18n down to react-i18next
    .init(
      {
        returnEmptyString: false,
        compatibilityJSON: "v3",
        lng: language_code,
        debug: false,
        supportedLngs: [
          "ar",
          "cs",
          "de",
          "el",
          "en",
          "es",
          "es_419",
          "fi",
          "fr",
          "it",
          "jpn_JP",
          "lt",
          "nb",
          "oc",
          "pl",
          "pt_BR",
          "pt",
          "ro",
          "sl",
          "ta",
          "uk",
        ],
        fallbackLng: {
          // In order to not download the fallback language translation file when it won't be needed, languages in which the application is fully translated should have an empty array for their key here
          fr: [],
          en: [],
          pt: [],
          default: ["en"],
        },
        load: "currentOnly",
        backend: translationsBackendOptions,
        interpolation: {
          escapeValue: false, // React already safes from XSS
        },
      },
      onInitialized,
    );

  if (onLanguageChanged) {
    i18next.on("languageChanged", onLanguageChanged);
  }
}

export { i18n_init };
export default i18n_init;
