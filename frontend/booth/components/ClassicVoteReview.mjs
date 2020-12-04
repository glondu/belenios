function TranslatableClassicVoteReview({ electionData=null, uncryptedBallot=[], t }){
  const renderedQuestions = electionData.questions.map(function(question, question_index){
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
            question.answers[index]
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
    return e(
      React.Fragment,
      null,
      e(
        "h3",
        {
          className: "review-encrypt-section__question-title"
        },
        question.question,
      ),
      e(
        "ul",
        {
          className: "classic-vote-review__answers-to-question"
        },
        ...renderedAnswers
      )
    );
  });
  return e(
    "div",
    {
      className: "classic-vote-review"
    },
    ...renderedQuestions
  );
}

const ClassicVoteReview = ReactI18next.withTranslation()(TranslatableClassicVoteReview);

export { ClassicVoteReview, TranslatableClassicVoteReview };
export default ClassicVoteReview;
