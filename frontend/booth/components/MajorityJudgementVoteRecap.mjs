import DisplayDependingOnWindowWidth from "./DisplayDependingOnWindowWidth.mjs";
import { majorityJudgementGradeIndexToCssColor } from "../majority_judgement_colors.mjs";

function MajorityJudgementVoteRecapForCandidateBig({ candidateName, selectedGradeName, selectedGradeNumber, availableGradesCssColors }){
  const bemBlockName = "majority-judgement-vote-recap-for-candidate-big";
  return e(
    "div",
    {
      className: bemBlockName,
      style : {
        '--majority-judgement-selected-grade-color': availableGradesCssColors[selectedGradeNumber]
      }
    },
    e(
      "div",
      {
        className: `${bemBlockName}__candidate-name`
      },
      candidateName
    ),
    e(
      "div",
      {
        className: `${bemBlockName}__selected-grade`
      },
      selectedGradeName
    )
  );
}

function MajorityJudgementVoteRecapForCandidateSmall({ candidateName, selectedGradeName, selectedGradeNumber, availableGradesCssColors }){
  const bemBlockName = "majority-judgement-vote-recap-for-candidate-small";
  return e(
    "div",
    {
      className: bemBlockName,
      style : {
        '--selected-grade-color': availableGradesCssColors[selectedGradeNumber]
      }
    },
    e(
      "div",
      {
        className: `${bemBlockName}__candidate-name`
      },
      candidateName
    ),
    e(
      "div",
      {
        className: `${bemBlockName}__selected-grade`
      },
      selectedGradeName
    )
  );
}

function MajorityJudgementVoteRecapForCandidate({ candidateName, selectedGradeName, selectedGradeNumber, availableGradesCssColors }){
  return e(
    DisplayDependingOnWindowWidth,
    {
      widthLimit: 800,
      smallComponent: MajorityJudgementVoteRecapForCandidateSmall,
      bigComponent: MajorityJudgementVoteRecapForCandidateBig,
      candidateName,
      selectedGradeName,
      selectedGradeNumber,
      availableGradesCssColors
    }
  );
}

function TranslatableMajorityJudgementVoteRecap({ question, question_index, uncryptedBallot, t }){
  const questionText = question.value.question;
  const questionCandidates = question.value.answers;
  const questionPossibleGrades = question.extra[1];
  const availableGradesCssColors = React.useMemo(() => {
    return questionPossibleGrades.map((grade, index) => {
      return majorityJudgementGradeIndexToCssColor(questionPossibleGrades.length, index);
    })
  }, questionPossibleGrades);
  const renderedGradedCandidates = uncryptedBallot[question_index].map(function(answer, answer_index){
    // TODO: verify that blank vote works just like in classic vote mode
    if (question.blank === true && answer_index === 0 && answer === 1){
      return e(
        "li",
        null,
        t("Blank vote")
      );
    }
    else {
      if (answer < 0 || answer >= questionPossibleGrades.length){
        console.error(`uncryptedBallot for question ${question_index} contains an answer for candidate ${answer_index} which is out of the available grades interval.`);
        return e(
          "li",
          null,
          "ERROR"
        );
      }
      const index = question.blank === true ? answer_index-1 : answer_index;
      return e(
        MajorityJudgementVoteRecapForCandidate,
        {
          candidateName: questionCandidates[index],
          selectedGradeName: questionPossibleGrades[answer],
          selectedGradeNumber: answer,
          availableGradesCssColors
        }
      );
    }
  });
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
        className: "majority-judgement-vote-recap__answers-to-question"
      },
      ...renderedGradedCandidates
    )
  );
  return e(
    "div",
    {
      className: "majority-judgement-vote-recap"
    },
    renderedVoteToQuestion
  );
}

const MajorityJudgementVoteRecap = ReactI18next.withTranslation()(TranslatableMajorityJudgementVoteRecap);

export { TranslatableMajorityJudgementVoteRecap, MajorityJudgementVoteRecap };
export default MajorityJudgementVoteRecap;
