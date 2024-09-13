import React, { createElement as e } from "react";
import { markup } from "../shortcuts.js";

function CandidateWithRadio({
  name = "radio-button-choice",
  id = "radio-button_1",
  value = "choice_1",
  checked = false,
  candidateInfo = "choice 1",
  dispatchUpdateUserVoteForCandidateInQuestion = () => {},
  currentAlertsForCandidateInQuestion = undefined,
  ...props
}) {
  const checkedValue = checked ? "checked" : null;
  const onChange = (event) => {
    dispatchUpdateUserVoteForCandidateInQuestion(
      event.target.checked === true ? true : false,
    );
  };
  let cssClasses = "candidate-with-checkbox clickable";
  if (currentAlertsForCandidateInQuestion) {
    cssClasses += " candidate-with-checkbox--with-alert";
  }
  return e(
    "div",
    {
      className: cssClasses,
      ...props,
    },
    e("input", {
      type: "radio",
      name: name,
      id: id,
      value: value,
      defaultChecked: checkedValue,
      onChange: onChange,
    }),
    e(
      "label",
      {
        htmlFor: id,
      },
      e("span", {
        className: "radio-button-appearance",
      }),
      e(
        "span",
        {
          className: "candidate-info",
        },
        markup(candidateInfo),
      ),
    ),
  );
}

export { CandidateWithRadio };
export default CandidateWithRadio;
