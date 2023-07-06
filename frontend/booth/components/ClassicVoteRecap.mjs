import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";
import { markup } from "../shortcuts.mjs";

function TranslatableClassicVoteRecap({
  question,
  question_index,
  uncryptedBallot,
  t,
}) {
  const questionText = question.title;
  const questionPossibleAnswers = question.answers;
  const renderedAnswers = uncryptedBallot[question_index].map(
    function (answer, answer_index) {
      if (answer === 0) {
        return null;
      } else if (answer === 1) {
        if (answer_index === 0 && question.blankVoteIsAllowed === true) {
          return e("li", null, t("blank_vote"));
        } else {
          const index =
            question.blankVoteIsAllowed === true
              ? answer_index - 1
              : answer_index;
          return e("li", null, markup(questionPossibleAnswers[index]));
        }
      } else {
        console.error(
          `uncryptedBallot for question ${question_index} contains an answer which is something else than 0 or 1.`,
        );
        return e("li", null, "ERROR");
      }
    },
  );
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
      "ul",
      {
        className: "classic-vote-recap__answers-to-question",
      },
      ...renderedAnswers,
    ),
  );
  return e(
    "div",
    {
      className: "classic-vote-recap",
    },
    renderedVoteToQuestion,
  );
}

const ClassicVoteRecap = withTranslation()(TranslatableClassicVoteRecap);

export { ClassicVoteRecap, TranslatableClassicVoteRecap };
export default ClassicVoteRecap;
