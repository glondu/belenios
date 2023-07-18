import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

import { QuestionTypeEnum } from "../election_utils.js";
import ClassicVoteRecap from "./ClassicVoteRecap.js";
import MajorityJudgmentVoteRecap from "./MajorityJudgmentVoteRecap.js";
import PreferentialVotingVoteRecap from "./PreferentialVotingVoteRecap.js";
import PreferentialVotingWithoutEqualityVoteRecap from "./PreferentialVotingWithoutEqualityVoteRecap.js";

function TranslatableWholeVoteRecap({
  electionObject = null,
  uncryptedBallot = [],
  t,
}) {
  const renderedQuestions = electionObject.questions.map(
    function (question, question_index) {
      const questionType = question.type;
      let questionRecapComponent;
      if (questionType === QuestionTypeEnum.MAJORITY_JUDGMENT) {
        questionRecapComponent = MajorityJudgmentVoteRecap;
      } else if (
        questionType === QuestionTypeEnum.PREFERENTIAL_VOTING_WITH_EQUALITY
      ) {
        questionRecapComponent = PreferentialVotingVoteRecap;
      } else if (
        questionType === QuestionTypeEnum.PREFERENTIAL_VOTING_WITHOUT_EQUALITY
      ) {
        questionRecapComponent = PreferentialVotingWithoutEqualityVoteRecap;
      } else if (questionType === QuestionTypeEnum.CLASSIC) {
        questionRecapComponent = ClassicVoteRecap;
      } else {
        return e("div", null, "Error: Unknown question type.");
      }
      return e(questionRecapComponent, {
        question,
        question_index,
        uncryptedBallot,
        t,
      });
    },
  );
  return e(
    "div",
    {
      className: "whole-vote-recap",
    },
    ...renderedQuestions,
  );
}

const WholeVoteRecap = withTranslation()(TranslatableWholeVoteRecap);

export { WholeVoteRecap, TranslatableWholeVoteRecap };
export default WholeVoteRecap;
