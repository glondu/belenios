const QuestionTypeEnum = Object.freeze({
  GENERIC: "GENERIC",
  CLASSIC: "CLASSIC", // In this question type, voter can select between `questions[i].min` and `questions[i].max` answers, or optionally vote blank (if `questions[i].blank` is true). Question's title is available as `questions[i].question`. Available answers or candidates are each element of array `questions[i].answers`.
  MAJORITY_JUDGMENT: "MAJORITY_JUDGMENT", // In this question type, voter must associate a grade (represented by a number) to each answer or candidate. Question's title is available as `questions[i].value.question`. Available answers or candidates are each element of array `questions[i].value.answers`.
  PREFERENTIAL_VOTING_WITH_EQUALITY: "PREFERENTIAL_VOTING_WITH_EQUALITY", // In this question type, voter must associate a rank (represented by a number, 1 being the most preferred, and a bigger number being less preferred ; blank vote cand be accepted, and equality is accepted) to each answer or candidate. Question's title is available as `questions[i].value.question`. Available answers or candidates are each element of array `questions[i].value.answers`
  PREFERENTIAL_VOTING_WITHOUT_EQUALITY: "PREFERENTIAL_VOTING_WITHOUT_EQUALITY", // In this question type, voter must associate a rank (represented by a number, 1 being the most preferred, and a bigger number being less preferred ; blank vote can be accepted, and equality is not accepted) to each answer or candidate. Question's title is available as `questions[i].value.question`. Available answers or candidates are each element of array `questions[i].value.answers`
  LISTS: "LISTS",
});

const detectQuestionType = (question) => {
  const lists = question.hasOwnProperty("type") && question["type"] == "Lists";
  const nonHomomorphic =
    question.hasOwnProperty("type") && question["type"] == "NonHomomorphic";
  if (lists) {
    return QuestionTypeEnum.LISTS;
  } else if (!nonHomomorphic) {
    return QuestionTypeEnum.CLASSIC;
  } else {
    if (
      question.hasOwnProperty("extra") &&
      question.extra.hasOwnProperty("type")
    ) {
      const questionSubType = question.extra.type;
      let preciseErrorText = `This booth does not know how to render questions of type "${questionSubType}".`;
      if (questionSubType == "ScoreVoting") {
        return QuestionTypeEnum.MAJORITY_JUDGMENT;
      } else if (questionSubType == "PreferentialVoting") {
        if (!question.extra.hasOwnProperty("method")) {
          preciseErrorText = `This booth does not know how to render questions of type "${questionSubType}" and which have no "method" attribute.`;
          throw preciseErrorText;
        }
        const questionMethod = question.extra.method;
        if (questionMethod == "Schulze") {
          return QuestionTypeEnum.PREFERENTIAL_VOTING_WITH_EQUALITY;
        } else if (questionMethod == "STV") {
          return QuestionTypeEnum.PREFERENTIAL_VOTING_WITHOUT_EQUALITY;
        } else {
          preciseErrorText = `This booth does not know how to render questions of type "${questionSubType}" and which "method" attribute is "${questionMethod}".`;
          throw preciseErrorText;
        }
      } else {
        throw preciseErrorText;
      }
    } else {
      return QuestionTypeEnum.GENERIC;
    }
  }
};

class ElectionQuestion {
  constructor(questionData) {
    this.questionData = questionData;
    this.type = detectQuestionType(this.questionData);
    if (
      this.type === QuestionTypeEnum.MAJORITY_JUDGMENT ||
      this.type === QuestionTypeEnum.PREFERENTIAL_VOTING_WITH_EQUALITY ||
      this.type === QuestionTypeEnum.PREFERENTIAL_VOTING_WITHOUT_EQUALITY
    ) {
      this.title = this.questionData.value.question;
      this.answers = this.questionData.value.answers;
      this.candidates = this.questionData.value.answers;
      this.blankVoteIsAllowed =
        "extra" in this.questionData &&
        "blank" in this.questionData.extra &&
        this.questionData.extra.blank === true;
      if (this.type === QuestionTypeEnum.MAJORITY_JUDGMENT) {
        this.availableGrades = this.questionData.extra.grades;
      }
    } else if (this.type === QuestionTypeEnum.CLASSIC) {
      this.title = this.questionData.question;
      this.answers = this.questionData.answers;
      this.blankVoteIsAllowed =
        "blank" in this.questionData && this.questionData["blank"] === true;
      this.min = this.questionData.min;
      this.max = this.questionData.max;
    } else if (this.type === QuestionTypeEnum.LISTS) {
      this.title = this.questionData.value.question;
      this.answers = this.questionData.value.answers;
    } else if (this.type === QuestionTypeEnum.GENERIC) {
      this.title = this.questionData.value.question;
      this.answers = this.questionData.value.answers;
    } else {
      // TODO
    }
  }
}

class Election {
  constructor(electionData) {
    this.electionData = electionData;
    this.title = electionData.name;
    this.description = electionData.description;
    this.uuid = electionData.uuid;
    this.questions = electionData.questions.map((questionData) => {
      return new ElectionQuestion(questionData);
    });
  }
}

export { QuestionTypeEnum, detectQuestionType, ElectionQuestion, Election };
export default Election;
