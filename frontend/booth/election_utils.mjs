const QuestionTypeEnum = Object.freeze(
  {
    "CLASSIC": "CLASSIC", // In this question type, voter can select between `questions[i].min` and `questions[i].max` answers, or optionally vote blank (if `questions[i].blank` is true). Question's title is available as `questions[i].question`. Available answers or candidates are each element of array `questions[i].answers`.
    "MAJORITY_JUDGMENT": "MAJORITY_JUDGMENT", // In this question type, voter must associate a grade (represented by a number) to each answer or candidate. Question's title is available as `questions[i].value.question`. Available answers or candidates are each element of array `questions[i].value.answers`.
    "PREFERENTIAL_VOTING": "PREFERENTIAL_VOTING" // In this question type, voter must associate a rank (represented by a number, 1 being the most preferred, and a bigger number being less preferred ; blank vote and equality are accepted) to each answer or candidate. Question's title is available as `questions[i].value.question`. Available answers or candidates are each element of array `questions[i].value.answers`
  }
);

const detectQuestionType = (question) => {
  const nonHomomorphic = question.hasOwnProperty("type") && question["type"] == "NonHomomorphic";
  if(!nonHomomorphic){
    return QuestionTypeEnum.CLASSIC;
  }
  else {
    const basicErrorText = "This booth does not know how to render non-homomorphic questions which don't mention their type.";
    if(question.hasOwnProperty("extra") && question.extra.hasOwnProperty("type")){
      const questionSubType = question.extra.type;
      const preciseErrorText = `This booth does not know how to render questions of type "${questionSubType}".`;
      if (questionSubType == "ScoreVoting"){
        return QuestionTypeEnum.MAJORITY_JUDGMENT;
      }
      else if (questionSubType == "PreferentialVoting"){ // TODO: verify type name, once backend transmits this piece of information
        return QuestionTypeEnum.PREFERENTIAL_VOTING;
      }
      else {
        throw preciseErrorText;
      }
    }
    else {
      throw basicErrorText; 
    }
  }
}

class ElectionQuestion {
  constructor(questionData) {
    this.questionData = questionData;
    this.type = detectQuestionType(this.questionData);
    if (this.type === QuestionTypeEnum.MAJORITY_JUDGMENT || this.type === QuestionTypeEnum.PREFERENTIAL_VOTING){
      this.title = this.questionData.value.question;
      this.answers = this.questionData.value.answers;
      this.candidates = this.questionData.value.answers;
      this.blankVoteIsAllowed = "extra" in this.questionData && "blank" in this.questionData.extra && this.questionData.extra.blank === true;
      if (this.type === QuestionTypeEnum.MAJORITY_JUDGMENT){
        this.availableGrades = this.questionData.extra.grades;
      }
    }
    else if (this.type === QuestionTypeEnum.CLASSIC){
      this.title = this.questionData.question;
      this.answers = this.questionData.answers;
      this.blankVoteIsAllowed = "blank" in this.questionData && this.questionData["blank"] === true;
      this.min = this.questionData.min;
      this.max = this.questionData.max;
    }
    else {
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
    this.questions = electionData.questions.map(
      (questionData) => {
        return new ElectionQuestion(questionData);
      }
    );
  } 
}

export { QuestionTypeEnum, detectQuestionType, ElectionQuestion, Election };
export default Election;
