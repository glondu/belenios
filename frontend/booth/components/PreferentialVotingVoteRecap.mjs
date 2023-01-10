import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

import { buildColumnLabel } from "./PreferentialVotingCandidatesList.mjs";

function PreferentialVotingVoteRecapForPreferenceLevel({ preference_level_title, preference_level_candidates }){
  if (preference_level_candidates.length === 0){
    return null;
  }
  const rendered_candidates = preference_level_candidates.map((candidate) => {
    return e(
      'div',
      {
        className: 'preferential-voting-vote-recap__candidate'
      },
      candidate
    );
  });
  return e(
    'div',
    {
      className: 'preferential-voting-vote-recap__preference-level'
    },
    e(
      'div',
      {
        className: 'preferential-voting-vote-recap__preference-level__title'
      },
      preference_level_title
    ),
    e(
      'div',
      {
        className: 'preferential-voting-vote-recap__preference-level__candidates'
      },
      ...rendered_candidates
    )
  );
}

function TranslatablePreferentialVotingVoteRecap({ question, question_index, uncryptedBallot, t }){
  const questionText = question.title;
  const questionCandidates = question.candidates;
  const questionPossibleGrades = question.availableGrades;
  let renderedGradedCandidates = [];
  if(question.blankVoteIsAllowed === true && uncryptedBallot[question_index].reduce(
    (accumulator, value) => {
      if(value !== 0){
        accumulator += 1;
      }
      return accumulator;
    }
  , 0) === 0){
    renderedGradedCandidates = [e(
      "div",
      {
        style: {
          paddingLeft: "40px"
        }
      },
      t("blank_vote")
    )];
  }
  else {
    let candidatesIndexesGroupedByPreferenceLevel = [];
    let notRankedCandidatesIndexes = [];
    uncryptedBallot[question_index].forEach(function(candidate_selected_ranking, candidate_index){
      if (candidate_selected_ranking === 0){
        notRankedCandidatesIndexes.push(candidate_index);
      }
      const selectedPreferenceLevelIndex = candidate_selected_ranking-1; // We substract 1 in order to obtain the index of the selected grade in the array of available grades labels (indexes in arrays start at 0, and by convention index 0 must contain the label of the highest grade, index 2 must contain the label of the second highest grade, etc), whereas the value of answer in the uncrypted ballot represent the selected grade encoded as Belenios backend expects it, which is: grades are expected to start at 1, 1 being the highest grade, 2 being the second highest grade, etc (and 0 being interpreted as invalid vote, whereas 0 to all candidates of a question being interpreted as blank vote).
      if(!Array.isArray(candidatesIndexesGroupedByPreferenceLevel[selectedPreferenceLevelIndex])){
        candidatesIndexesGroupedByPreferenceLevel[selectedPreferenceLevelIndex] = [];
      }
      candidatesIndexesGroupedByPreferenceLevel[selectedPreferenceLevelIndex].push(candidate_index);
    });

    renderedGradedCandidates = candidatesIndexesGroupedByPreferenceLevel.map((preference_level_candidates, preference_level_index) => {
      return e(
        PreferentialVotingVoteRecapForPreferenceLevel,
        {
          preference_level_title: buildColumnLabel(null, preference_level_index, t),
          preference_level_candidates: preference_level_candidates.map((candidate_index) => questionCandidates[candidate_index])
        }
      );
    });

    if (notRankedCandidatesIndexes.length > 0){
      renderedGradedCandidates.push(
        e(
          PreferentialVotingVoteRecapForPreferenceLevel,
          {
            preference_level_title: t("preferential_voting_not_ranked"),
            preference_level_candidates: notRankedCandidatesIndexes.map((candidate_index) => questionCandidates[candidate_index])
          }
        )
      );
    }
  }
  const renderedVoteToQuestion = e(
    React.Fragment,
    null,
    e(
      "h3",
      {
        className: "whole-vote-recap__question-title"
      },
      questionText,
    ),
    e(
      "div",
      {
        className: "preferential-voting-vote-recap__answers-to-question"
      },
      ...renderedGradedCandidates
    )
  );
  return e(
    "div",
    {
      className: "preferential-voting-vote-recap"
    },
    renderedVoteToQuestion
  );
}

const PreferentialVotingVoteRecap = withTranslation()(TranslatablePreferentialVotingVoteRecap);

export { TranslatablePreferentialVotingVoteRecap, PreferentialVotingVoteRecap };
export default PreferentialVotingVoteRecap;
