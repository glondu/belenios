function TranslatableClassicVoteRecap({ question, question_index, uncryptedBallot, t }){
  const questionText = question.question;
  const questionPossibleAnswers = question.answers;
  const renderedAnswers = uncryptedBallot[question_index].map(function(answer, answer_index){
    if(answer === 0){
      return null;
    }
    else if(answer === 1){
      if(answer_index === 0 && question.blank === true){
        return e(
          "li",
          null,
          t("Blank vote")
        );
      }
      else {
        const index = question.blank === true ? answer_index-1 : answer_index;
        return e(
          "li",
          null,
          questionPossibleAnswers[index]
        );
      }
    }
    else {
      console.error(`uncryptedBallot for question ${question_index} contains an answer which is something else than 0 or 1.`);
      return e(
        "li",
        null,
        "ERROR"
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
      "ul",
      {
        className: "classic-vote-recap__answers-to-question"
      },
      ...renderedAnswers
    )
  );
  return e(
    "div",
    {
      className: "classic-vote-recap"
    },
    renderedVoteToQuestion
  );
}

const ClassicVoteRecap = ReactI18next.withTranslation()(TranslatableClassicVoteRecap);

export { ClassicVoteRecap, TranslatableClassicVoteRecap };
export default ClassicVoteRecap;
