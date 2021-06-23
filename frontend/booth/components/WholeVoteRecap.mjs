import { QuestionTypeEnum } from "../election_utils.mjs";
import ClassicVoteRecap from "./ClassicVoteRecap.mjs";
import MajorityJudgmentVoteRecap from "./MajorityJudgmentVoteRecap.mjs";
import PreferentialVotingVoteRecap from "./PreferentialVotingVoteRecap.mjs";

function TranslatableWholeVoteRecap({ electionObject=null, uncryptedBallot=[], t }){
  const renderedQuestions = electionObject.questions.map(function(question, question_index){
    const questionType = question.type;
    let questionRecapComponent;
    if (questionType === QuestionTypeEnum.MAJORITY_JUDGMENT){
      questionRecapComponent = MajorityJudgmentVoteRecap;
    }
    else if (questionType === QuestionTypeEnum.PREFERENTIAL_VOTING){
      questionRecapComponent = PreferentialVotingVoteRecap;
    }
    else if (questionType === QuestionTypeEnum.CLASSIC){
      questionRecapComponent = ClassicVoteRecap;
    }
    else {
      return e(
        "div",
        null,
        "Error: Unknown question type."
      );
    }
    return e(
      questionRecapComponent,
      {
        question,
        question_index,
        uncryptedBallot,
        t
      }
    );
  });
  return e(
    "div",
    {
      className: "whole-vote-recap"
    },
    ...renderedQuestions
  );
}

const WholeVoteRecap = ReactI18next.withTranslation()(TranslatableWholeVoteRecap);

export { WholeVoteRecap, TranslatableWholeVoteRecap };
export default WholeVoteRecap;
