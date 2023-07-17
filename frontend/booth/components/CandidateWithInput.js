import React, { createElement as e } from "react";
import { markup } from "../shortcuts.js";

function CandidateWithInput({
  name,
  id,
  value,
  candidateInfo,
  dispatchUpdateUserVoteForCandidateInQuestion,
  currentAlertsForCandidateInQuestion,
  ...props
}) {
  const onChange = (event) => {
    dispatchUpdateUserVoteForCandidateInQuestion(event.target.value);
  };
  let cssClasses = "candidate-with-input clickable";
  if (currentAlertsForCandidateInQuestion) {
    cssClasses += " candidate-with-input--with-alert";
  }
  return e(
    "div",
    {
      className: cssClasses,
      ...props,
    },
    e("input", {
      type: "number",
      name: name,
      id: id,
      value: value,
      onChange: onChange,
    }),
    e(
      "label",
      {
        htmlFor: id,
      },
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

CandidateWithInput.defaultProps = {
  name: "input-choice",
  id: "input_1",
  value: 0,
  candidateInfo: "choice 1",
  dispatchUpdateUserVoteForCandidateInQuestion: () => {},
  currentAlertsForCandidateInQuestion: undefined,
};

export { CandidateWithInput };
export default CandidateWithInput;
