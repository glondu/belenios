import { QuestionTypeEnum, detectQuestionType, QuestionWithVotableAnswers } from "./QuestionWithVotableAnswers.mjs";
import VoteNavigation from "./VoteNavigation.mjs";

/* We chose to not use a `<form>`, because it could increase possibilities to leak voter's choices. Instead, we use `<input type="checkbox">` or `<input type="radio">` fields outside of a `<form>`, and classic `<button>` for navigation between questions ("Previous" and "Next" labels). */
function TranslatableAllQuestionsWithPagination(props){
  const [current_question_index, set_current_question_index] = React.useState(props.current_question_index);
  const initialVoteForAllQuestions = props.electionData.questions.map((question, question_index) => {
    const questionType = detectQuestionType(question);
    if (questionType == QuestionTypeEnum.MAJORITY_JUDGEMENT){
      return question.value.answers.map((answer, answer_index) => {
        return undefined;
      });
    }
    else if (questionType == QuestionTypeEnum.CLASSIC){
      return question.answers.map((answer, answer_index) => {
        return undefined;
      });
    }
    return [];
  });
  const deepCloneArray = (currentArray) => {
    return currentArray.map((arr) => {
      return arr.slice();
    });
  };
  const currentUserVoteForAllQuestionsReducer = (state, action) => {
    let updatedVoteToAllQuestions;
    switch(action.type){
      case 'saveVoteForCandidateInQuestion':
        updatedVoteToAllQuestions = deepCloneArray(state);
        updatedVoteToAllQuestions[action.question_index][action.candidate_index] = action.user_vote_for_candidate;
        return updatedVoteToAllQuestions;
        break;
      case 'saveVoteForCandidateInQuestionAndResetOthers':
        updatedVoteToAllQuestions = deepCloneArray(state);
        updatedVoteToAllQuestions[action.question_index] = updatedVoteToAllQuestions[action.question_index].map((el) => { return undefined; });
        updatedVoteToAllQuestions[action.question_index][action.candidate_index] = action.user_vote_for_candidate;
        return updatedVoteToAllQuestions;
        break;
      default:
        throw new Error();
    }
  };
  const [current_user_vote_for_all_questions, dispatch_current_user_vote_for_all_questions] = React.useReducer(currentUserVoteForAllQuestionsReducer, initialVoteForAllQuestions);
  // TODO: handle local saving of blank vote, probably in another data structure?

  const extractVoterSelectedAnswersFromState = () => {
    /*
    Type of vote_of_voter_per_question: Array where each ith element corresponds to voter's vote on question i.
    - If type of ith question is classic checkbox, voter's vote to this question is an array of integers, where the jth element is respectively 0 or 1 when the jth answer is respectively not checked or checked.
    - If type of ith question is classic radio, voter's vote to this question is an array of integers, where all elements are 0 except the jth element which is 1, as the jth answer radio button is ticked.
    - If type of ith questionis majority judgement, voter's vote to this question is an array of integers, where each jth element value corresponds to the integer version of the grade selected by the voter for the jth candidate.
    If blank vote is allowed on the ith question, then an element is added to the beginning of the ith array, with a value of respectively 1 or 0 if the user has repectively decided to vote blank on this question or not.
    */
    let vote_of_voter_per_question = [];
    vote_of_voter_per_question = props.electionData.questions.map(function(question, question_index){
      let answers_to_question = [];
      const questionType = detectQuestionType(question);
      if (questionType == QuestionTypeEnum.MAJORITY_JUDGEMENT){
        let question_answers = question.value.answers;
        answers_to_question = current_user_vote_for_all_questions[question_index].slice(0, question_answers.length).map((el) => {return el === undefined ? 0 : el;});
        // TODO: handle blank vote
      }
      else if (questionType === QuestionTypeEnum.CLASSIC){
        let question_answers = question.answers;
        answers_to_question = current_user_vote_for_all_questions[question_index].slice(0, question_answers.length).map((el) => {return el === undefined ? 0 : el;});
        // if blank vote is allowed on this answer, then the blank value must be placed at the beginning
        if ("blank" in question && question["blank"] === true){
          const voter_has_voted_blank = (current_user_vote_for_all_questions[question_index].length == question_answers.length + 1) && (current_user_vote_for_all_questions[question_index][question_answers.length] === 1) ? 1 : 0;
          answers_to_question = [voter_has_voted_blank, ...answers_to_question];
        }
      }
      return answers_to_question;
    });
    return vote_of_voter_per_question;
  };

  const bindFunctionMergeObjectToFirstParameter = (f, obj) => {
    return (obj2) => {
      return f({...obj2, ...obj});
    };
  };
  
  const scrollToTopOfPage = () => {
    window.scrollTo(0, 0);
  };

  React.useEffect(() => {
    scrollToTopOfPage();
  }, [current_question_index]);

  const onClickPrevious = () => {
    if (current_question_index-1 >= 0){
      set_current_question_index(current_question_index-1);
    }
  };

  const onClickNext = (event) => {
    const t = props.t;
    // Before moving on to next question, verify that user input respects question constraints:
    // - if blank vote is allowed on this question and user voted blank, then verify that no other answer is checked
    // - if this question accepts between X and Y answers and user has not voted blank, verify that user has not checked less than X answers, nor more than Y answers
    const current_question_data = props.electionData.questions[current_question_index];
    const voter_selected_answers = extractVoterSelectedAnswersFromState();
    const number_of_answers_checked = voter_selected_answers[current_question_index].reduce(
      function(accumulator, value, index){
        const answer_value = value === 1 ? 1 : 0;
        return accumulator + answer_value;
      },
      0
    );
    if(current_question_data.blank === true && voter_selected_answers[current_question_index][0] === 1){
      if(number_of_answers_checked > 1){
        alert(t("questionConstraintNoBlankAndOther"));
        return;
      }
    }
    else {
      if(number_of_answers_checked < current_question_data.min){
        alert(t("questionConstraintNoLessThanMin", {count: current_question_data.min}));
        return;
      }
      if(number_of_answers_checked > current_question_data.max){
        alert(t("questionConstraintNoMoreThanMax", {count: current_question_data.max}));
        return;
      }
    }

    if (current_question_index+1 < props.electionData.questions.length){
      set_current_question_index(current_question_index+1);
    }
    else {
      if (props.onVoteSubmit){
        return props.onVoteSubmit(event, props.electionData, voter_selected_answers);
      }
    }
  }

  const renderedQuestions = props.electionData.questions.map(function(question, question_index){
    const questionType = detectQuestionType(question);
    let answers;
    let minimumAnswers = null;
    let maximumAnswers = null;
    let questionText = null;
    let blankVoteAllowed = null;
    let complementaryProps = {};
    const identifierPrefix = `question_${question_index}_`;
    const visible = current_question_index === question_index ? true : false;
    if (questionType === QuestionTypeEnum.MAJORITY_JUDGEMENT){
      answers = question.value.answers;
      questionText = question.value.question;
      
      // Receive from backend the number of available grades, their labels and their ordering (index 0 is the best grade, and appreciation gets worse as index increases)
      if ("extra" in question && Array.isArray(question.extra) && question.extra.length > 1 && question.extra[0] == "MajorityJudgment" && Array.isArray(question.extra[1]) && question.extra[1].length > 1){
        complementaryProps.availableGrades = question.extra[1];
      }
      else {
        return e(
          "div",
          null,
          "Error: Wrong election parameters. Question of type MajorityJudgment does not provide the list of votable mentions."
        );
      }
    }
    else if (questionType === QuestionTypeEnum.CLASSIC){
      answers = question.answers;
      questionText = question.question;
      minimumAnswers = question.min;
      maximumAnswers = question.max;
      blankVoteAllowed = question.blank;
    }

    return e(
      QuestionWithVotableAnswers,
      {
        questionType: questionType,
        answers: answers,
        minimumAnswers: minimumAnswers,
        maximumAnswers: maximumAnswers,
        question: questionText,
        blankVoteAllowed: blankVoteAllowed,
        identifierPrefix: identifierPrefix,
        visible: visible,
        currentUserVoteForQuestion: current_user_vote_for_all_questions[question_index],
        dispatchUpdateUserVoteForQuestion: bindFunctionMergeObjectToFirstParameter(dispatch_current_user_vote_for_all_questions,
          {'question_index': question_index}
        ),
        ...complementaryProps
      }
    )
  });

  const renderedPagination = e(
    VoteNavigation,
    {
      question_index: current_question_index,
      questions_length: props.electionData.questions.length,
      onClickPreviousButton: onClickPrevious,
      onClickNextButton: onClickNext
    }
  );
  return e(
    React.Fragment,
    null,
    ...renderedQuestions,
    renderedPagination
  );
}

TranslatableAllQuestionsWithPagination.defaultProps = {
  current_question_index: 0,
  electionData: null,
  onVoteSubmit: null,
  t: function(s){ return s; }
};

const AllQuestionsWithPagination = ReactI18next.withTranslation()(TranslatableAllQuestionsWithPagination);

export { AllQuestionsWithPagination, TranslatableAllQuestionsWithPagination, QuestionTypeEnum, detectQuestionType };
export default AllQuestionsWithPagination;
