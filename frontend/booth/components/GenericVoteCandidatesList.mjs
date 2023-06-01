import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

import CandidateWithInput from "./CandidateWithInput.mjs";

function TranslatableGenericVoteCandidatesList({
  candidates,
  identifierPrefix,
  currentUserVoteForQuestion,
  currentCandidatesHavingAlertsForQuestion,
  dispatchUpdateUserVoteForQuestion,
  t,
}) {
  const renderedCandidates = candidates.map((candidate, candidateIndex) => {
    const identifier = `${identifierPrefix}_choice_${candidateIndex}`;
    const dispatchUpdateUserVoteForCandidateInQuestion = (candidate_value) => {
      dispatchUpdateUserVoteForQuestion({
        type: "saveVoteForCandidateInQuestion",
        candidate_index: candidateIndex,
        user_vote_for_candidate: candidate_value,
      });
    };
    const currentAlert =
      currentCandidatesHavingAlertsForQuestion &&
      currentCandidatesHavingAlertsForQuestion.includes(candidateIndex);
    const commonProps = {
      candidateInfo: candidate,
      value: currentUserVoteForQuestion[candidateIndex],
      id: identifier,
      key: candidateIndex,
      dispatchUpdateUserVoteForCandidateInQuestion,
      currentAlertsForCandidateInQuestion: currentAlert,
    };
    const additionalProps = {
      name: identifier,
    };
    return e(CandidateWithInput, {
      ...commonProps,
      ...additionalProps,
    });
  });

  return e(
    "div",
    {
      className: "generic-vote-candidates-list noselect",
    },
    ...renderedCandidates,
  );
}

TranslatableGenericVoteCandidatesList.defaultProps = {
  identifierPrefix: "question_1",
  candidates: ["Answer 1", "Answer 2", "Answer 3"],
  currentUserVoteForQuestion: [],
  currentCandidatesHavingAlertsForQuestion: [],
  dispatchUpdateUserVoteForQuestion: () => {},
  t: (s) => {
    return s;
  },
};

const GenericVoteCandidatesList = withTranslation()(
  TranslatableGenericVoteCandidatesList,
);

export { GenericVoteCandidatesList, TranslatableGenericVoteCandidatesList };
export default GenericVoteCandidatesList;
