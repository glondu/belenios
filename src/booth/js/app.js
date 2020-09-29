'use strict';

const likeButtonContainer = document.querySelector('#like_button_container');
ReactDOM.render(e(LikeButton), likeButtonContainer);

function changeLanguageTo(language){
  i18next.changeLanguage(language);
};

function changeLanguageToEnglish(){
  changeLanguageTo('en');
};

function changeLanguageToFrench(){
  changeLanguageTo('fr');
};

function LanguageSelector(){
  return e(
    "div",
    null,
    e(
      "button",
      {
        "onClick": changeLanguageToEnglish
      },
      "english"
    ),
    e(
      "button",
      {
        "onClick": changeLanguageToFrench
      },
      "français"
    )
   );
};

const languageContainer = document.querySelector("#language-selector-container");
ReactDOM.render(e(LanguageSelector), languageContainer);

const translatedLikeButtonContainer = document.querySelector('#translated_like_button_container');
ReactDOM.render(e(TranslatedLikeButton), translatedLikeButtonContainer);
