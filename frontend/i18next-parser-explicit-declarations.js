/*
i18next-parser does not seem to detect translation keys which are used only as values of other translation keys.
So we declare these translation keys here to make sure they don't get deleted from JSON translation files when we regenerate them from Javascript source files.
*/

const s1 = t("ask_to_select_between_x");
const s2 = t("question_x");
