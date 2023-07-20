import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";
import { markup } from "../shortcuts.js";

function PreferentialVotingWithoutEqualityVoteRecapForPreferenceLevel({
  preference_level_title,
  preference_level_candidates,
}) {
  if (preference_level_candidates.length === 0) {
    return null;
  }
  const rendered_candidates = preference_level_candidates.map((candidate) => {
    return e(
      "div",
      {
        className: "preferential-voting-vote-recap__candidate",
      },
      candidate,
    );
  });
  return e(
    "div",
    {
      className: "preferential-voting-vote-recap__preference-level",
    },
    e(
      "div",
      {
        className: "preferential-voting-vote-recap__preference-level__title",
      },
      preference_level_title,
    ),
    e(
      "div",
      {
        className:
          "preferential-voting-vote-recap__preference-level__candidates",
      },
      ...rendered_candidates,
    ),
  );
}

function TranslatablePreferentialVotingWithoutEqualityVoteRecap({
  question,
  question_index,
  uncryptedBallot,
  t,
}) {
  const questionText = question.title;
  const questionCandidates = question.candidates;
  let renderedGradedCandidates = [];
  if (
    question.blankVoteIsAllowed === true &&
    uncryptedBallot[question_index].reduce((accumulator, value) => {
      if (value !== 0) {
        accumulator += 1;
      }
      return accumulator;
    }, 0) === 0
  ) {
    renderedGradedCandidates = [
      e(
        "div",
        {
          style: {
            paddingLeft: "40px",
          },
        },
        t("blank_vote"),
      ),
    ];
  } else {
    let notRankedCandidatesIndexes = [];
    let rankedCandidates = [];
    uncryptedBallot[question_index].forEach(
      function (candidate_selected_ranking, candidate_index) {
        if (candidate_selected_ranking === 0) {
          notRankedCandidatesIndexes.push(candidate_index);
        } else {
          const selectedPreferenceLevelIndex = candidate_selected_ranking - 1;
          rankedCandidates[selectedPreferenceLevelIndex] = `${
            selectedPreferenceLevelIndex + 1
          }. ${questionCandidates[candidate_index]}`;
        }
      },
    );

    if (rankedCandidates.length > 0) {
      renderedGradedCandidates.push(
        e(PreferentialVotingWithoutEqualityVoteRecapForPreferenceLevel, {
          preference_level_title:
            notRankedCandidatesIndexes.length == 0
              ? ""
              : t("preferential_voting_ranked"),
          preference_level_candidates: rankedCandidates,
        }),
      );
    }

    if (notRankedCandidatesIndexes.length > 0) {
      renderedGradedCandidates.push(
        e(PreferentialVotingWithoutEqualityVoteRecapForPreferenceLevel, {
          preference_level_title: t("preferential_voting_not_ranked"),
          preference_level_candidates: notRankedCandidatesIndexes.map(
            (candidate_index) => questionCandidates[candidate_index],
          ),
        }),
      );
    }
  }
  const renderedVoteToQuestion = e(
    React.Fragment,
    null,
    e(
      "h3",
      {
        className: "whole-vote-recap__question-title",
      },
      markup(questionText),
    ),
    e(
      "div",
      {
        className: "preferential-voting-vote-recap__answers-to-question",
      },
      ...renderedGradedCandidates,
    ),
  );
  return e(
    "div",
    {
      className: "preferential-voting-vote-recap",
    },
    renderedVoteToQuestion,
  );
}

const PreferentialVotingWithoutEqualityVoteRecap = withTranslation()(
  TranslatablePreferentialVotingWithoutEqualityVoteRecap,
);

export {
  TranslatablePreferentialVotingWithoutEqualityVoteRecap,
  PreferentialVotingWithoutEqualityVoteRecap,
};
export default PreferentialVotingWithoutEqualityVoteRecap;
