import DisplayDependingOnWindowWidth from "./DisplayDependingOnWindowWidth.mjs";
import { majorityJudgmentGradeIndexToCssColor } from "../majority_judgment_colors.mjs";

function MajorityJudgmentVoteRecapForCandidateBig({ candidateName, selectedGradeName, selectedGradeNumber, availableGradesCssColors }){
  const bemBlockName = "majority-judgment-vote-recap-for-candidate-big";
  return e(
    "div",
    {
      className: bemBlockName,
      style : {
        '--majority-judgment-selected-grade-color': availableGradesCssColors[selectedGradeNumber]
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

function MajorityJudgmentVoteRecapForCandidateSmall({ candidateName, selectedGradeName, selectedGradeNumber, availableGradesCssColors }){
  const bemBlockName = "majority-judgment-vote-recap-for-candidate-small";
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

function MajorityJudgmentVoteRecapForCandidate({ candidateName, selectedGradeName, selectedGradeNumber, availableGradesCssColors }){
  return e(
    DisplayDependingOnWindowWidth,
    {
      widthLimit: 800,
      smallComponent: MajorityJudgmentVoteRecapForCandidateSmall,
      bigComponent: MajorityJudgmentVoteRecapForCandidateBig,
      candidateName,
      selectedGradeName,
      selectedGradeNumber,
      availableGradesCssColors
    }
  );
}

function TranslatableMajorityJudgmentVoteRecap({ question, question_index, uncryptedBallot, t }){
  const questionText = question.value.question;
  const questionCandidates = question.value.answers;
  const questionPossibleGrades = question.extra[1];
  const availableGradesCssColors = React.useMemo(() => {
    return questionPossibleGrades.map((grade, index) => {
      return majorityJudgmentGradeIndexToCssColor(questionPossibleGrades.length, index);
    })
  }, questionPossibleGrades);
  const renderedGradedCandidates = uncryptedBallot[question_index].map(function(answer, answer_index){
    const selectedGradeIndex = answer-1; // We substract 1 in order to obtain the index of the selected grade in the array of available grades labels (indexes in arrays start at 0, and by convention index 0 must contain the label of the highest grade, index 2 must contain the label of the second highest grade, etc), whereas the value of answer in the uncrypted ballot represent the selected grade encoded as Belenios backend expects it, which is: grades are expected to start at 1, 1 being the highest grade, 2 being the second highest grade, etc (and 0 being interpreted as the lowest grade). 
    if (selectedGradeIndex < 0 || selectedGradeIndex >= questionPossibleGrades.length){
      console.error(`uncryptedBallot for question ${question_index} contains an answer for candidate ${answer_index} which is out of the available grades interval.`);
      return e(
        "li",
        null,
        "ERROR"
      );
    }
    const index = question.blank === true ? answer_index-1 : answer_index;
    return e(
      MajorityJudgmentVoteRecapForCandidate,
      {
        candidateName: questionCandidates[index],
        selectedGradeName: questionPossibleGrades[selectedGradeIndex],
        selectedGradeNumber: selectedGradeIndex,
        availableGradesCssColors
      }
    );
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
        className: "majority-judgment-vote-recap__answers-to-question"
      },
      ...renderedGradedCandidates
    )
  );
  return e(
    "div",
    {
      className: "majority-judgment-vote-recap"
    },
    renderedVoteToQuestion
  );
}

const MajorityJudgmentVoteRecap = ReactI18next.withTranslation()(TranslatableMajorityJudgmentVoteRecap);

export { TranslatableMajorityJudgmentVoteRecap, MajorityJudgmentVoteRecap };
export default MajorityJudgmentVoteRecap;
