import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";
import { markup } from "../shortcuts.js";

function TranslatableListsVoteRecap({
  question,
  question_index,
  uncryptedBallot,
  t,
}) {
  const questionText = question.title;
  const questionPossibleAnswers = question.answers;
  const renderedAnswers = uncryptedBallot[question_index].map(
    function (listAnswer, listIndex) {
      if (listAnswer[0] === 0) {
        // list is not selected
        return null;
      } else if (listAnswer[0] === 1) {
        // list is selected
        const candidates = listAnswer
          .slice(1)
          .map((candidateAnswer, candidateIndex) => {
            return e(
              "div",
              {
                className:
                  "lists-vote-recap__candidate" +
                  (candidateAnswer === 1
                    ? ""
                    : " lists-vote-recap__candidate-strikethrough"),
              },
              markup(questionPossibleAnswers[listIndex][candidateIndex + 1]),
            );
          });
        return e(
          "li",
          null,
          markup(questionPossibleAnswers[listIndex][0]),
          ...candidates,
        );
      } else {
        console.error(
          `uncryptedBallot for question ${question_index} contains an invalid answer.`,
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
        className: "lists-vote-recap__answers-to-question",
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

const ListsVoteRecap = withTranslation()(TranslatableListsVoteRecap);

export { ListsVoteRecap, TranslatableListsVoteRecap };
export default ListsVoteRecap;
