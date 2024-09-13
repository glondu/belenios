import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

import DisplayDependingOnWindowWidth from "./DisplayDependingOnWindowWidth.js";
import { TranslatableMajorityJudgmentVoteSmallCandidatesList } from "./MajorityJudgmentVoteSmallCandidatesList.js";
import { TranslatableMajorityJudgmentVoteBigCandidatesList } from "./MajorityJudgmentVoteBigCandidatesList.js";
import { majorityJudgmentGradeIndexToCssColor } from "../majority_judgment_colors.js";
import CandidateWithCheckbox from "./CandidateWithCheckbox.js";

function TranslatableMajorityJudgmentVoteCandidatesList({
  identifierPrefix = "question_1",
  availableGrades = ["Poor", "Good", "Excellent"],
  candidates = ["Candidate 1", "Candidate 2", "Candidate 3"],
  blankVoteIsAllowed = false,
  currentUserVoteForQuestion,
  currentCandidatesHavingAlertsForQuestion = [],
  dispatchUpdateUserVoteForQuestion = () => {},
  t = (s) => {
    return s;
  },
}) {
  const availableGradesCssColors = React.useMemo(() => {
    return availableGrades.map((grade, index) => {
      return majorityJudgmentGradeIndexToCssColor(
        availableGrades.length,
        index,
      );
    });
  }, availableGrades);
  let renderedBlankVoteComponent = null;
  const candidateIndex = candidates.length;
  const userHasSelectedBlankVote =
    blankVoteIsAllowed &&
    currentUserVoteForQuestion.length > candidates.length &&
    currentUserVoteForQuestion[candidateIndex] === 1
      ? true
      : false;
  if (blankVoteIsAllowed) {
    const blankVoteLabel = t("blank_vote");
    const identifier = `${identifierPrefix}_blank-vote`;
    const currentAlerts =
      currentCandidatesHavingAlertsForQuestion &&
      currentCandidatesHavingAlertsForQuestion.includes(candidateIndex);
    const dispatchBlankVoteInQuestion = (blankVoteIsChecked) => {
      dispatchUpdateUserVoteForQuestion({
        type: "saveBlankVoteInQuestion",
        blankVoteIsChecked,
      });
    };
    const commonProps = {
      candidateInfo: blankVoteLabel,
      checked: userHasSelectedBlankVote,
      id: identifier,
      key: candidateIndex,
      dispatchUpdateUserVoteForCandidateInQuestion: dispatchBlankVoteInQuestion,
      currentAlertsForCandidateInQuestion: currentAlerts,
      name: identifier,
    };
    const blankVoteProps = {
      style: {
        margin: "50px auto 30px",
        maxWidth: "400px",
      },
    };
    renderedBlankVoteComponent = e(CandidateWithCheckbox, {
      ...commonProps,
      ...blankVoteProps,
    });
  }
  let cssClasses = "majority-judgment-vote-candidates-list noselect";
  if (userHasSelectedBlankVote) {
    cssClasses +=
      " majority-judgment-vote-candidates-list--blank-vote-is-selected";
  }
  return e(
    "div",
    {
      className: cssClasses,
    },
    e(DisplayDependingOnWindowWidth, {
      widthLimit: 800,
      smallComponent: TranslatableMajorityJudgmentVoteSmallCandidatesList,
      bigComponent: TranslatableMajorityJudgmentVoteBigCandidatesList,
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
    }),
  );
}

const MajorityJudgmentVoteCandidatesList = withTranslation()(
  TranslatableMajorityJudgmentVoteCandidatesList,
);

export {
  MajorityJudgmentVoteCandidatesList,
  TranslatableMajorityJudgmentVoteCandidatesList,
};
export default MajorityJudgmentVoteCandidatesList;
