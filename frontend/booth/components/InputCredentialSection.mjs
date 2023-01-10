import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

import { BlueNiceButton } from "./NiceButton.mjs";
import { NiceTextInput } from "./NiceInput.mjs";

function TranslatableInputCredentialSection({ onSubmit=null, t }){
  const credentialId = "credential";
  const onClick = () => {
    const result = document.getElementById(credentialId).value.trim();
    if (result && onSubmit){
      return onSubmit(result);
    }
  };
  return e(
    "div",
    {
      className: "input-credential-section-container",
      style: {
        padding: "30px"
      }
    },
    e(
      "div",
      {
        className: "input-credential-section",
        style: {
          background: "rgb(229, 242, 247)",
          padding: "20px",
          textAlign: "center",
          borderRadius: "8px"
        }
      },
      e(
        "div",
        {
          className: "input-credential-section__instruction-container",
          style: {
            maxWidth: "300px",
            margin: "0 auto"
          }
        },
        e(
          "div",
          {
            className: "input-credential-section__instruction",
            style: {
              paddingBottom: "12px"
            }
          },
          e(
            "p",
            null,
            t("ask_for_credential")
          ),
          e(
            NiceTextInput,
            {
              id: credentialId,
              onKeyUp: function(e) {
                  if (e.keyCode == 13) onClick();
              }
            }
          )
        ),
        e(
          BlueNiceButton,
          {
            className: "input-credential-section__button",
            style: {
              padding: "8px 28px"
            },
            label: t("next_button_label"),
            onClick: onClick
          }
        )
      )
    )
  );
}

const InputCredentialSection = withTranslation()(TranslatableInputCredentialSection);

export { InputCredentialSection, TranslatableInputCredentialSection };
export default InputCredentialSection;
