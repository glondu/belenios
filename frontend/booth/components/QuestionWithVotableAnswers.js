import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";
import { markup } from "../shortcuts.js";

import { TranslatableGenericVoteCandidatesList } from "./GenericVoteCandidatesList.js";
import { TranslatableListsVoteCandidatesList } from "./ListsVoteCandidatesList.js";
import { TranslatableClassicVoteCandidatesList } from "./ClassicVoteCandidatesList.js"; // FIXME: We have to import TranslatableClassicVoteCandidatesList instead of ClassicVoteCandidatesList, because otherwise Storybook throws a hook error.
import { TranslatableMajorityJudgmentVoteCandidatesList } from "./MajorityJudgmentVoteCandidatesList.js";
import { TranslatablePreferentialVotingCandidatesList } from "./PreferentialVotingCandidatesList.js";
import { TranslatablePreferentialVotingWithoutEqualityCandidatesList } from "./PreferentialVotingWithoutEqualityCandidatesList.js";
import { QuestionTypeEnum, detectQuestionType } from "../election_utils.js";

function TranslatableQuestionWithVotableAnswers({
  question,
  identifierPrefix = "question_1_",
  visible = true,
  currentUserVoteForQuestion = [],
  currentAlertsTextsForQuestion = [],
  currentCandidatesHavingAlertsForQuestion = [],
  dispatchUpdateUserVoteForQuestion = () => {},
  t,
}) {
  let description;
  let rendered_answers;

  if (question.type === QuestionTypeEnum.MAJORITY_JUDGMENT) {
    description = t("majority_judgment_question_description");
    rendered_answers = e(TranslatableMajorityJudgmentVoteCandidatesList, {
      identifierPrefix,
      candidates: question.answers,
      blankVoteIsAllowed: question.blankVoteIsAllowed,
      availableGrades: question.availableGrades,
      currentUserVoteForQuestion,
      currentCandidatesHavingAlertsForQuestion,
      dispatchUpdateUserVoteForQuestion,
      t,
    });
  } else if (
    question.type === QuestionTypeEnum.PREFERENTIAL_VOTING_WITH_EQUALITY
  ) {
    description = t("preferential_voting_question_description");
    rendered_answers = e(TranslatablePreferentialVotingCandidatesList, {
      identifierPrefix,
      candidates: question.answers,
      blankVoteIsAllowed: question.blankVoteIsAllowed,
      currentUserVoteForQuestion,
      currentCandidatesHavingAlertsForQuestion,
      dispatchUpdateUserVoteForQuestion,
      t,
    });
  } else if (
    question.type === QuestionTypeEnum.PREFERENTIAL_VOTING_WITHOUT_EQUALITY
  ) {
    description = t("preferential_voting_question_description");
    rendered_answers = e(
      TranslatablePreferentialVotingWithoutEqualityCandidatesList,
      {
        identifierPrefix,
        candidates: question.answers,
        blankVoteIsAllowed: question.blankVoteIsAllowed,
        currentUserVoteForQuestion,
        currentCandidatesHavingAlertsForQuestion,
        dispatchUpdateUserVoteForQuestion,
        t,
      },
    );
  } else if (question.type === QuestionTypeEnum.CLASSIC) {
    let minimumAnswers = question.min;
    let maximumAnswers = question.max;
    let classic_question_subtype = "checkbox";
    if (minimumAnswers === 1 && maximumAnswers === 1) {
      classic_question_subtype = "radio";
    }
    if (minimumAnswers === maximumAnswers) {
      description = t("ask_to_select_x_answers", { count: minimumAnswers });
    } else {
      description = t("ask_to_select_between_x_and_y_answers", {
        min: minimumAnswers,
        count: maximumAnswers,
      });
    }
    rendered_answers = e(TranslatableClassicVoteCandidatesList, {
      type: classic_question_subtype,
      identifierPrefix,
      candidates: question.answers,
      blankVoteIsAllowed: question.blankVoteIsAllowed,
      currentUserVoteForQuestion,
      currentCandidatesHavingAlertsForQuestion,
      dispatchUpdateUserVoteForQuestion,
      t,
    });
  } else if (question.type === QuestionTypeEnum.LISTS) {
    description = t("lists_question_description");
    rendered_answers = e(TranslatableListsVoteCandidatesList, {
      identifierPrefix,
      lists: question.answers,
      currentUserVoteForQuestion,
      currentCandidatesHavingAlertsForQuestion,
      dispatchUpdateUserVoteForQuestion,
      t,
    });
  } else if (question.type === QuestionTypeEnum.GENERIC) {
    rendered_answers = e(TranslatableGenericVoteCandidatesList, {
      identifierPrefix,
      candidates: question.answers,
      currentUserVoteForQuestion,
      currentCandidatesHavingAlertsForQuestion,
      dispatchUpdateUserVoteForQuestion,
      t,
    });
  }
  const bemBlockName = "question-with-votable-answers";
  const containerClassNames = visible
    ? bemBlockName
    : `${bemBlockName} ${bemBlockName}--hidden`;
  const alertsElements = currentAlertsTextsForQuestion.reduce(
    (accumulator, value) => {
      if (value) {
        accumulator.push(e("p", null, value));
      }
      return accumulator;
    },
    [],
  );
  return e(
    "div",
    {
      className: containerClassNames,
    },
    e(
      "h3",
      {
        className: `${bemBlockName}__question-title`,
      },
      markup(question.title),
    ),
    e(
      "p",
      {
        className: `${bemBlockName}__question-description`,
      },
      description,
    ),
    rendered_answers,
    e(
      "div",
      {
        className: `${bemBlockName}__alerts`,
      },
      ...alertsElements,
    ),
  );
}

const QuestionWithVotableAnswers = withTranslation()(
  TranslatableQuestionWithVotableAnswers,
);

export { QuestionWithVotableAnswers, TranslatableQuestionWithVotableAnswers };
export default QuestionWithVotableAnswers;
