import React, { createElement as e } from "react";
import { markup } from "../shortcuts.js";
import CandidateWithCheckbox from "./CandidateWithCheckbox.js";

function CandidateList({
  name,
  id,
  value,
  listName,
  listCandidates,
  dispatchUpdateUserVoteForCandidateInQuestion,
  identifierPrefix,
  ...props
}) {
  let selected = value[0] === 1;

  const onClick = () => {
    if (!selected) {
      let newValue = value.map((_) => 1);
      dispatchUpdateUserVoteForCandidateInQuestion(newValue);
    }
  };

  let cssClasses = "candidate-list";
  if (!selected) {
    cssClasses += " clickable";
  }

  let candidates = listCandidates.map((candidate, candidateIndex) => {
    const identifier = `${identifierPrefix}_candidate_${candidateIndex}`;
    // value contains list selection at index 0
    const checked = value[candidateIndex + 1] === 1 ? true : false;
    let dispatchCandidateUpdate = (checked) => {
      let newValue = value;
      newValue[candidateIndex + 1] = checked ? 1 : 0;
      dispatchUpdateUserVoteForCandidateInQuestion(newValue);
    };
    const commonProps = {
      candidateInfo: candidate,
      checked: checked,
      id: identifier,
      key: candidateIndex,
      dispatchUpdateUserVoteForCandidateInQuestion: dispatchCandidateUpdate,
      currentAlertsForCandidateInQuestion: false,
    };
    const additionalProps = {
      name: identifier,
    };
    return e(CandidateWithCheckbox, {
      ...commonProps,
      ...additionalProps,
    });
  });

  return e(
    "div",
    {
      className: cssClasses,
      onClick: onClick,
      ...props,
    },
    e(
      "div",
      {
        className: "candidate-list__list-name",
      },
      markup(listName),
    ),
    selected ? e("div", null, ...candidates) : null,
  );
}

CandidateList.defaultProps = {
  name: "input-choice",
  id: "input_1",
  value: 0,
  listName: "list 1",
  listCandidates: ["candidate 1"],
  dispatchUpdateUserVoteForCandidateInQuestion: () => {},
};

export { CandidateList };
export default CandidateList;
