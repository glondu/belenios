import { TranslatableClassicVoteCandidatesList } from "./ClassicVoteCandidatesList.mjs"; // FIXME: We have to import TranslatableClassicVoteCandidatesList instead of ClassicVoteCandidatesList, because otherwise Storybook throws a hook error.
import { TranslatableMajorityJudgementVoteCandidatesList } from "./MajorityJudgementVoteCandidatesList.mjs";

const QuestionTypeEnum = Object.freeze(
  {
    "CLASSIC": "CLASSIC", // In this question type, voter can select between `questions[i].min` and `questions[i].max` answers, or optionally vote blank (if `questions[i].blank` is true). Question's title is available as `questions[i].question`. Available answers or candidates are each element of array `questions[i].answers`.
    "MAJORITY_JUDGEMENT": "MAJORITY_JUDGEMENT" // In this question type, voter must associate a grade (represented by a number) to each answer or candidate. Question's title is available as `questions[i].value.question`. Available answers or candidates are each element of array `questions[i].value.answers`.
  }
);

const detectQuestionType = (question) => {
  const nonHomomorphic = question.hasOwnProperty("type") && question["type"] == "NonHomomorphic";
  if(!nonHomomorphic){
    return QuestionTypeEnum.CLASSIC;
  }
  else {
    const basicErrorText = 'Unhandled question type';
    if(question.hasOwnProperty("extra") && question.extra.length > 1){
      const questionSubType = question.extra[0];
      if (questionSubType == "MajorityJudgment"){
        return QuestionTypeEnum.MAJORITY_JUDGEMENT;
      }
      else if (questionSubType == "PreferenceOrdering"){ // TODO: verify type name, once backend transmits this piece of information
        throw basicErrorText; // TODO: return QuestionTypeEnum.PREFERENCE_ORDERING;
      }
    }
    else {
      throw basicErrorText; 
    }
  }
}

function TranslatableQuestionWithVotableAnswers({ questionType, minimumAnswers, maximumAnswers, question, answers, blankVoteAllowed, identifierPrefix, visible, currentUserVoteForQuestion, dispatchUpdateUserVoteForQuestion, availableGrades=null, t }){
  let description;
  let rendered_answers;
  if (questionType === QuestionTypeEnum.MAJORITY_JUDGEMENT){
    description = t("majorityJudgementQuestionDescription");
    rendered_answers = e(
      TranslatableMajorityJudgementVoteCandidatesList,
      {
        identifierPrefix,
        candidates: answers,
        availableGrades,
        currentUserVoteForQuestion,
        dispatchUpdateUserVoteForQuestion,
        t
      }
    );
  }
  else if (questionType === QuestionTypeEnum.CLASSIC){
    let classic_question_subtype = "checkbox";
    if ( minimumAnswers === 1 && maximumAnswers === 1){
      classic_question_subtype = "radio";
    }
    if ( minimumAnswers === maximumAnswers ){
      description = t("selectXAnswers", {count: minimumAnswers});
    }
    else {
      description = t("selectBetweenXAndYAnswers", {min: minimumAnswers, count: maximumAnswers});
    }
    rendered_answers = e(
      TranslatableClassicVoteCandidatesList,
      {
        type: classic_question_subtype,
        identifierPrefix,
        candidates: answers,
        blankVoteAllowed,
        currentUserVoteForQuestion,
        dispatchUpdateUserVoteForQuestion,
        t
      }
    );
  }
  
  const containerClassNames = visible ? "question-with-votable-answers" : "question-with-votable-answers question-with-votable-answers--hidden";
  return e(
    'div',
    {
      className: containerClassNames
    },
    e(
      "h3",
      {
        className: "question-with-votable-answers__question-title"
      },
      question
    ),
    e(
      "p",
      {
        className: "question-with-votable-answers__question-description"
      },
      description
    ),
    rendered_answers
  );
}

TranslatableQuestionWithVotableAnswers.defaultProps = {
  "questionType": QuestionTypeEnum.CLASSIC,
  "answers": [
    "Answer 1",
    "Answer 2",
    "Answer 3"
  ],
  "minimumAnswers": 1,
  "maximumAnswers": 2,
  "blankVoteAllowed": false,
  "question": "Question 1?",
  "identifierPrefix": "question_1_",
  "visible": true,
  "currentUserVoteForQuestion": [],
  "dispatchUpdateUserVoteForQuestion": () => {}
};

const QuestionWithVotableAnswers = ReactI18next.withTranslation()(TranslatableQuestionWithVotableAnswers);

export { QuestionTypeEnum, QuestionWithVotableAnswers, TranslatableQuestionWithVotableAnswers, detectQuestionType };
export default QuestionWithVotableAnswers;
