import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

import { BlueNiceButton } from "./NiceButton.js";
import { NiceTextInput } from "./NiceInput.js";

function TranslatableInputCredentialSection({ onSubmit = null, t }) {
  const credentialId = "credential";
  const onClick = () => {
    const result = document.getElementById(credentialId).value.trim();
    if (result && onSubmit) {
      return onSubmit(result);
    }
  };
  return e(
    "div",
    {
      className: "input-credential-section__container",
    },
    e(
      "div",
      {
        className: "input-credential-section",
      },
      e(
        "div",
        {
          className: "input-credential-section__instruction-container",
        },
        e(
          "div",
          {
            className: "input-credential-section__instruction",
          },
          e("p", null, t("ask_for_credential")),
          e(NiceTextInput, {
            id: credentialId,
            onKeyUp: function (e) {
              if (e.keyCode == 13) onClick();
            },
          }),
        ),
        e(BlueNiceButton, {
          className: "input-credential-section__button",
          label: t("next_button_label"),
          onClick: onClick,
        }),
      ),
    ),
  );
}

const InputCredentialSection = withTranslation()(
  TranslatableInputCredentialSection,
);

export { InputCredentialSection, TranslatableInputCredentialSection };
export default InputCredentialSection;
