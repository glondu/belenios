function TranslatableMajorityJudgementVoteRecap({ question, question_index, uncryptedBallot, t }){
  const questionText = question.value.question;
  const questionCandidates = question.value.answers;
  const questionPossibleGrades = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']; // TODO: use real mentions
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
      const answerTextSelectedByUser = questionCandidates[index] + ' : ' + questionPossibleGrades[answer]; // TODO: implement nicer UI
      return e(
        "li",
        null,
        answerTextSelectedByUser
      );
    }
  });
  const renderedVoteToQuestion = e(
    React.Fragment,
    null,
    e(
      "h3",
      {
        className: "review-encrypt-section__question-title"
      },
      questionText,
    ),
    e(
      "ul",
      {
        className: "classic-vote-review__answers-to-question"
      },
      ...renderedGradedCandidates
    )
  );
  return e(
    "div",
    {
      className: "whole-vote-recap" // TODO: have its own classname and associated CSS
    },
    renderedVoteToQuestion
  );
}

const MajorityJudgementVoteRecap = ReactI18next.withTranslation()(TranslatableMajorityJudgementVoteRecap);

export { TranslatableMajorityJudgementVoteRecap, MajorityJudgementVoteRecap };
export default MajorityJudgementVoteRecap;
