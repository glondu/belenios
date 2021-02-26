import { QuestionTypeEnum, detectQuestionType } from "./QuestionWithVotableAnswers.mjs";
import ClassicVoteRecap from "./ClassicVoteRecap.mjs";
import MajorityJudgementVoteRecap from "./MajorityJudgementVoteRecap.mjs";

function TranslatableWholeVoteRecap({ electionData=null, uncryptedBallot=[], t }){
  const renderedQuestions = electionData.questions.map(function(question, question_index){
    const questionType = detectQuestionType(question);
    if (questionType == QuestionTypeEnum.MAJORITY_JUDGEMENT){
      return e(
        MajorityJudgementVoteRecap,
        {
          question,
          question_index,
          uncryptedBallot,
          t
        }
      );
    }
    else if (questionType == QuestionTypeEnum.CLASSIC){
      return e(
        ClassicVoteRecap,
        {
          question,
          question_index,
          uncryptedBallot,
          t
        }
      );
    }
    else {
      return e(
        "div",
        null,
        "Error: Unknown question type."
      );
    }
  });
  return e(
    "div",
    {
      className: "whole-vote-recap" // TODO: rename to "whole-vote-recap" and in CSS files too
    },
    ...renderedQuestions
  );
}

const WholeVoteRecap = ReactI18next.withTranslation()(TranslatableWholeVoteRecap);

export { WholeVoteRecap, TranslatableWholeVoteRecap };
export default WholeVoteRecap;
