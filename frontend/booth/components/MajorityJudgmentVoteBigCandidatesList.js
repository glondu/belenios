import React, { createElement as e } from "react";
import { markup } from "../shortcuts.js";

function MajorityJudgmentVoteBigCandidateAvailableGrade({
  name = "radion-button-choice",
  id = "radio-button_1",
  checked = false,
  gradeLabel = "Excellent",
  availableGrades = ["Poor", "Good", "Excellent"],
  gradeIndex = 2,
  dispatchUserVoteForCandidateInQuestion = (selection_grade_index) => {},
  availableGradesCssColors = ["red", "yellow", "green"],
  ...props
}) {
  const checkedValue = checked ? "checked" : null;
  const onChange = (event) => {
    if (event.target.checked) {
      dispatchUserVoteForCandidateInQuestion(gradeIndex);
    }
  };
  return e(
    "div",
    {
      className: `majority-judgment-vote-big-candidate__grade clickable`,
      style: {
        "--majority-judgment-available-grade-color":
          availableGradesCssColors[gradeIndex],
      },
      ...props,
    },
    e("input", {
      type: "radio",
      className: "majority-judgment-vote-big-candidate__grade__input",
      name: name,
      id: id,
      value: gradeIndex,
      defaultChecked: checkedValue,
      onChange: onChange,
    }),
    e(
      "label",
      {
        htmlFor: id,
        className: "majority-judgment-vote-big-candidate__grade__label",
      },
      e("span", {
        className: "radio-button-appearance",
      }),
      e(
        "span",
        {
          className:
            "majority-judgment-vote-big-candidate__grade__label__label",
        },
        gradeLabel,
      ),
    ),
  );
}

function TranslatableMajorityJudgmentVoteBigCandidate({
  candidateInfo,
  availableGrades,
  identifierPrefix,
  name,
  currentAlertsForCandidateInQuestion,
  dispatchUserVoteForCandidateInQuestion,
  selectedGradeIndex,
  availableGradesCssColors,
  t,
}) {
  const bemBlockName = "majority-judgment-vote-big-candidate";
  let cssClasses = bemBlockName;
  if (currentAlertsForCandidateInQuestion) {
    cssClasses += ` ${bemBlockName}--with-alert`;
  }
  const renderedGrades = availableGrades.map((gradeLabel, gradeIndex) => {
    const identifier = `${identifierPrefix}_grade_${gradeIndex}`;
    return e(MajorityJudgmentVoteBigCandidateAvailableGrade, {
      name,
      id: identifier,
      checked: selectedGradeIndex === gradeIndex,
      gradeLabel,
      availableGrades,
      gradeIndex,
      dispatchUserVoteForCandidateInQuestion,
      availableGradesCssColors,
    });
  });
  return e(
    "div",
    {
      className: cssClasses,
    },
    e(
      "div",
      {
        className: `${bemBlockName}__candidate-info`,
      },
      e(
        "div",
        {
          className: `${bemBlockName}__candidate-info__label`,
        },
        markup(candidateInfo),
      ),
    ),
    e(
      "div",
      {
        className: `${bemBlockName}__available-grades`,
      },
      ...renderedGrades,
    ),
  );
}

function TranslatableMajorityJudgmentVoteBigCandidatesList({
  identifierPrefix,
  candidates,
  blankVoteIsAllowed,
  renderedBlankVoteComponent,
  availableGrades,
  currentUserVoteForQuestion,
  currentCandidatesHavingAlertsForQuestion,
  dispatchUpdateUserVoteForQuestion,
  availableGradesCssColors,
  t,
}) {
  const shouldDisplayWideMode =
    availableGrades.length > 8 ||
    availableGrades.reduce((accumulator, gradeLabel) => {
      return accumulator + gradeLabel.length;
    }, 0) > 70;
  let renderedCandidates = candidates.map((candidate, candidateIndex) => {
    const identifierConsolidatedPrefix = `${identifierPrefix}_candidate_${candidateIndex}`;
    const currentAlerts =
      currentCandidatesHavingAlertsForQuestion &&
      currentCandidatesHavingAlertsForQuestion.includes(candidateIndex);
    const dispatchUserVoteForCandidateInQuestion = (selected_grade_index) => {
      dispatchUpdateUserVoteForQuestion({
        type: "saveVoteForCandidateInQuestion",
        candidate_index: candidateIndex,
        user_vote_for_candidate: selected_grade_index,
      });
    };
    return e(TranslatableMajorityJudgmentVoteBigCandidate, {
      candidateInfo: candidate,
      availableGrades,
      key: candidateIndex,
      identifierPrefix: identifierConsolidatedPrefix,
      name: identifierConsolidatedPrefix,
      currentAlertsForCandidateInQuestion: currentAlerts,
      dispatchUserVoteForCandidateInQuestion,
      selectedGradeIndex: currentUserVoteForQuestion[candidateIndex],
      availableGradesCssColors,
      t,
    });
  });

  if (renderedBlankVoteComponent) {
    renderedCandidates.push(renderedBlankVoteComponent);
  }

  let cssClasses = "majority-judgment-vote-big-candidates-list noselect";
  if (shouldDisplayWideMode) {
    cssClasses += " majority-judgment-vote-big-candidates-list--wide";
  }
  let approximateCandidatesListHeight = 100 * candidates.length;
  if (blankVoteIsAllowed) {
    approximateCandidatesListHeight += 100;
  }
  let afterCandidatesList = [];
  if (shouldDisplayWideMode) {
    afterCandidatesList = [
      e("div", {
        className: "majority-judgment-vote-big-candidates-list-spacer",
        style: {
          height: `${approximateCandidatesListHeight}px`,
        },
      }),
    ];
  }
  return e(
    React.Fragment,
    null,
    e(
      "div",
      {
        className: cssClasses,
      },
      ...renderedCandidates,
    ),
    ...afterCandidatesList,
  );
}

export { TranslatableMajorityJudgmentVoteBigCandidatesList };
export default TranslatableMajorityJudgmentVoteBigCandidatesList;
