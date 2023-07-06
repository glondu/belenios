import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

import { QuestionWithVotableAnswers } from "./QuestionWithVotableAnswers.mjs";
import { QuestionTypeEnum, detectQuestionType } from "../election_utils.mjs";
import VoteNavigation from "./VoteNavigation.mjs";

const deepCloneArray = (currentArray) => {
  return currentArray.map((element) => {
    if (Array.isArray(element)) {
      return element.slice();
    } else if (typeof element === "object") {
      return Object.assign({}, element);
    } else {
      return element;
    }
  });
};

const bindFunctionMergeObjectToFirstParameter = (f, obj) => {
  return (obj2) => {
    return f({ ...obj2, ...obj });
  };
};

/* We chose to not use a `<form>`, because it could increase possibilities to leak voter's choices. Instead, we use `<input type="checkbox">` or `<input type="radio">` fields outside of a `<form>`, and classic `<button>` for navigation between questions ("Previous" and "Next" labels). */
function TranslatableAllQuestionsWithPagination(props) {
  // --------------------
  // Definition of component state properties (and their reducers)
  // --------------------

  // Definition of state for current question index
  const [current_question_index, set_current_question_index] = React.useState(
    props.current_question_index,
  );

  // Definition of state for current alerts for all questions
  const initialAlertsForAllQuestions = props.electionObject.questions.map(
    (question, question_index) => {
      return {};
    },
  );
  const currentAlertsForAllQuestionsReducer = (state, action) => {
    let updatedAlertsForAllQuestions;
    switch (action.type) {
      case "saveAlertForCandidateInQuestion":
        updatedAlertsForAllQuestions = deepCloneArray(state);
        updatedAlertsForAllQuestions[action.question_index][action.alert_id] = {
          text: action.alert_text,
          candidate_indexes: action.candidates_indexes,
        };
        return updatedAlertsForAllQuestions;
        break;
      case "resetAlertInQuestion":
        updatedAlertsForAllQuestions = deepCloneArray(state);
        delete updatedAlertsForAllQuestions[action.question_index][
          action.alert_id
        ];
        return updatedAlertsForAllQuestions;
        break;
      case "resetAllAlertsInQuestion":
        updatedAlertsForAllQuestions = deepCloneArray(state);
        updatedAlertsForAllQuestions[action.question_index] = {};
        return updatedAlertsForAllQuestions;
        break;
      default:
        throw new Error();
    }
  };
  const [
    current_alerts_for_all_questions,
    dispatch_current_alerts_for_all_questions,
  ] = React.useReducer(
    currentAlertsForAllQuestionsReducer,
    initialAlertsForAllQuestions,
  );
  const candidatesIndexesHavingAlertInQuestion = React.useMemo(() => {
    return current_alerts_for_all_questions.map(
      (alerts_for_question, question_index) => {
        const indexes_dict = Object.values(alerts_for_question).reduce(
          (accumulator, value, index) => {
            if (value.candidate_indexes) {
              value.candidate_indexes.forEach((element) => {
                accumulator[element] = 1;
              });
            }
            return accumulator;
          },
          {},
        );
        const res = Object.keys(indexes_dict).map((el) => {
          return parseInt(el, 10);
        });
        return res;
      },
    );
  }, current_alerts_for_all_questions);
  const alertTextsInQuestion = React.useMemo(() => {
    return current_alerts_for_all_questions.map(
      (alerts_for_question, question_index) => {
        return Object.values(alerts_for_question).map((element) => {
          return element.text;
        });
      },
    );
  }, current_alerts_for_all_questions);

  // Definition of state for current vote to all questions
  // Variable `current_user_vote_for_all_questions` (and its initial state `initialVoteForAllQuestions`) is an array where the `ith` element corresponds to (initial) vote to question `i`. This element is itself an array where the `jth` element correponds to user's (initial) vote to the `jth` "available answer" (in classic questions) or "candidate" (in majority judgment), with `undefined` as initial value.
  // Then, if user votes blank to the `ith` question, an extra element is appended to array `current_user_vote_for_all_questions[i]`, containing `1`, so we have `current_user_vote_for_all_questions[i][question_answers.length] == 1`.
  // This current vote state can then ben converted into an uncrypted ballot using function `convertStateToUncryptedBallot()`.
  const computeInitialVoteToQuestionFromAvailableAnswersMapFunction = (
    answer,
    answer_index,
  ) => {
    return undefined;
  };
  const initialVoteForAllQuestions = props.electionObject.questions.map(
    (question, question_index) => {
      const questionType = question.type;
      if (questionType in QuestionTypeEnum) {
        return question.answers.map(
          computeInitialVoteToQuestionFromAvailableAnswersMapFunction,
        );
      }
      return [];
    },
  );
  const currentUserVoteForAllQuestionsReducer = (state, action) => {
    let updatedVoteToAllQuestions;
    switch (action.type) {
      case "saveVoteForCandidateInQuestion":
        updatedVoteToAllQuestions = deepCloneArray(state);
        updatedVoteToAllQuestions[action.question_index][
          action.candidate_index
        ] = action.user_vote_for_candidate;
        dispatch_current_alerts_for_all_questions({
          type: "resetAllAlertsInQuestion",
          question_index: action.question_index,
        });
        return updatedVoteToAllQuestions;
        break;
      case "saveVoteForAllCandidatesInQuestion":
        updatedVoteToAllQuestions = deepCloneArray(state);
        updatedVoteToAllQuestions[action.question_index] =
          action.user_vote_for_all_candidates_in_question;
        dispatch_current_alerts_for_all_questions({
          type: "resetAllAlertsInQuestion",
          question_index: action.question_index,
        });
        return updatedVoteToAllQuestions;
        break;
      case "saveBlankVoteInQuestion":
        updatedVoteToAllQuestions = deepCloneArray(state);
        updatedVoteToAllQuestions[action.question_index][
          props.electionObject.questions[action.question_index].answers.length
        ] = action.blankVoteIsChecked ? 1 : 0;
        return updatedVoteToAllQuestions;
        break;
      case "saveVoteForCandidateInQuestionAndResetOthers":
        updatedVoteToAllQuestions = deepCloneArray(state);
        updatedVoteToAllQuestions[action.question_index] =
          updatedVoteToAllQuestions[action.question_index].map((el) => {
            return undefined;
          });
        updatedVoteToAllQuestions[action.question_index][
          action.candidate_index
        ] = action.user_vote_for_candidate;
        dispatch_current_alerts_for_all_questions({
          type: "resetAllAlertsInQuestion",
          question_index: action.question_index,
        });
        return updatedVoteToAllQuestions;
        break;
      default:
        throw new Error();
    }
  };
  const [
    current_user_vote_for_all_questions,
    dispatch_current_user_vote_for_all_questions,
  ] = React.useReducer(
    currentUserVoteForAllQuestionsReducer,
    initialVoteForAllQuestions,
  );
  // Voter's current vote at current state of completion is stored in variable `current_user_vote_for_all_questions` (to all questions), including the information about whether or not he voted blank for a question. An idea of refactoring is to split information about voter's blank votes to another variable which would have its own data structure.

  // --------------------
  // End of definition of component state
  // --------------------

  const convertStateToUncryptedBallot = () => {
    /*
    This function reads component state variable `current_user_vote_for_all_questions` and produces a rearranged array, because Belenios backend for example expects the representation of a blank vote for a classic question to be [1,0,0,0,0] (1 and `n` zeros, where `n` is the number of available answers), and expects a blank vote for majority judgment question to be [0,0,0,0] (`n` zeros, where `n` is the number of candidates).
    Type of vote_of_voter_per_question: Array where each ith element corresponds to voter's vote on question i.
    - If type of ith question is classic checkbox, voter's vote to this question is an array of integers, where the jth element is respectively 0 or 1 when the jth answer is respectively not checked or checked.
    - If type of ith question is classic radio, voter's vote to this question is an array of integers, where all elements are 0 except the jth element which is 1, as the jth answer radio button is ticked.
    - If type of ith question is majority judgment, voter's vote to this question is an array of integers, where each jth element value corresponds to the integer version of the grade selected by the voter for the jth candidate.
    If blank vote is allowed on the ith question of type "classic", then an element is added to the beginning of the ith array, with a value of respectively 1 or 0 if the user has repectively decided to vote blank on this question or not.
    If blank vote is allowed on the ith question of type "majority judgment", and user has selected the blank vote for this question, then users' vote to this question is represented by an array of `n` zeros, where `n` is the number of candidates.
    */
    let vote_of_voter_per_question = [];
    vote_of_voter_per_question = props.electionObject.questions.map(
      function (question, question_index) {
        let answers_to_question = [];
        const questionType = question.type;
        if (
          questionType === QuestionTypeEnum.MAJORITY_JUDGMENT ||
          questionType === QuestionTypeEnum.PREFERENTIAL_VOTING_WITH_EQUALITY ||
          questionType === QuestionTypeEnum.PREFERENTIAL_VOTING_WITHOUT_EQUALITY
        ) {
          let question_answers = question.answers;
          // Handle blank vote: if (on this question) blank vote is allowed and user has voted blank, then we represent user's vote (to this question) as an array of zeros of length `question_answers.length`
          const user_has_voted_blank =
            question.blankVoteIsAllowed &&
            current_user_vote_for_all_questions[question_index].length ===
              question.answers.length + 1 &&
            current_user_vote_for_all_questions[question_index][
              question.answers.length
            ] === 1;
          if (user_has_voted_blank) {
            answers_to_question = question_answers.map((el) => {
              return 0;
            });
          } else {
            answers_to_question = current_user_vote_for_all_questions[
              question_index
            ]
              .slice(0, question_answers.length)
              .map((el) => {
                return el === undefined ? 0 : el + 1;
              }); // We add 1 because the value of el represents the index of the selected grade in the array of available grades labels (indexes in arrays start at 0, and by convention index 0 must contain the label of the highest grade, index 2 must contain the label of the second highest grade, etc), whereas Belenios backend expects Majority Judgement grades to start at 1, 1 being the highest grade, 2 being the second highest grade, etc (and 0 being interpreted as "vote nul" in French (invalid vote), and voting 0 to every candidate being interpreted as voting blank to this question). And Belenios backend expects Preferential Voting rank associated to each candidate to also start at 1, 1 being the most preferred (and 0 being interpreted as "not ranked", and voting 0 to every candidate being interpreted as voting blank to this question).

            if (
              questionType ===
              QuestionTypeEnum.PREFERENTIAL_VOTING_WITH_EQUALITY
            ) {
              // remove all preference levels which are empty, because the backend only cares about relative ordering
              const upperBound = question.answers.length;
              let aCandidateAtCurrentPreferenceLevelExists;
              let hasChanged;
              do {
                hasChanged = false;
                for (
                  let preferenceLevel = upperBound;
                  preferenceLevel > 0;
                  --preferenceLevel
                ) {
                  aCandidateAtCurrentPreferenceLevelExists =
                    answers_to_question.find(
                      (level) => level === preferenceLevel,
                    );
                  if (!aCandidateAtCurrentPreferenceLevelExists) {
                    answers_to_question = answers_to_question.map((level) => {
                      if (level > preferenceLevel) {
                        hasChanged = true;
                        return level - 1;
                      } else {
                        return level;
                      }
                    });
                  }
                }
              } while (hasChanged);
            }
            // TODO: Is there anything special to do in case of QuestionTypeEnum.PREFERENTIAL_VOTING_WITHOUT_EQUALITY?
          }
        } else if (questionType === QuestionTypeEnum.CLASSIC) {
          let question_answers = question.answers;
          answers_to_question = current_user_vote_for_all_questions[
            question_index
          ]
            .slice(0, question_answers.length)
            .map((el) => {
              return el === undefined ? 0 : el;
            });
          // Handle blank vote: if blank vote is allowed on this question, then the blank value (1 if voter has voted blank, else 0) must be placed at the beginning of the vote array for this question
          if (question.blankVoteIsAllowed) {
            const voter_has_voted_blank =
              current_user_vote_for_all_questions[question_index].length ==
                question_answers.length + 1 &&
              current_user_vote_for_all_questions[question_index][
                question_answers.length
              ] === 1
                ? 1
                : 0;
            answers_to_question = [
              voter_has_voted_blank,
              ...answers_to_question,
            ];
          }
        }
        return answers_to_question;
      },
    );
    return vote_of_voter_per_question;
  };

  const scrollToTopOfPage = () => {
    window.scrollTo(0, 0);
  };

  React.useEffect(() => {
    scrollToTopOfPage();
  }, [current_question_index]);

  const onClickPrevious = () => {
    if (current_question_index - 1 >= 0) {
      set_current_question_index(current_question_index - 1);
    }
  };

  // When user clicks on the Next button:
  // - convert component state to uncrypted ballot
  // - verify coherence/validity of user input on this question
  // - if input is valid:
  //   - if another question exists after this one:
  //     - display next question
  //   - else:
  //     - submit vote to encryption and recap step
  // - else:
  //   - display validity errors on current screen
  const onClickNext = (event) => {
    const t = props.t;
    const current_question_data =
      props.electionObject.questions[current_question_index];
    const voter_selected_answers_as_uncrypted_ballot =
      convertStateToUncryptedBallot();
    const questionType = current_question_data.type;
    let user_vote_to_question_is_valid = true;
    if (questionType === QuestionTypeEnum.MAJORITY_JUDGMENT) {
      // Verify that user has selected a grade for all candidates (in majority judgment, it is not accepted to select a grade for only some candidates)
      const user_has_voted_blank =
        current_question_data.blankVoteIsAllowed &&
        current_user_vote_for_all_questions[current_question_index].length >
          current_question_data.answers.length &&
        current_user_vote_for_all_questions[current_question_index][
          current_question_data.answers.length
        ] === 1;
      if (!user_has_voted_blank) {
        current_user_vote_for_all_questions[current_question_index].forEach(
          (selected_grade, candidate_index) => {
            if (selected_grade === undefined) {
              dispatch_current_alerts_for_all_questions({
                type: "saveAlertForCandidateInQuestion",
                alert_id: `shouldSelectGradeForCandidate_${candidate_index}`,
                question_index: current_question_index,
                candidates_indexes: [candidate_index],
                alert_text: t(
                  "majority_judgment_alert_grade_is_mandatory_for_candidate_x",
                  { candidate: candidate_index + 1 },
                ), // we could also display candidate name
              });
              user_vote_to_question_is_valid = false;
            }
          },
        );
      }
    } else if (
      questionType === QuestionTypeEnum.PREFERENTIAL_VOTING_WITH_EQUALITY
    ) {
      // TODO: Verify validity of a vote to a question of type `QuestionTypeEnum.PREFERENTIAL_VOTING_WITH_EQUALITY`. Is there a possibility for such a vote to be invalid? What could an invalid vote look like? What should we verify?
    } else if (
      questionType === QuestionTypeEnum.PREFERENTIAL_VOTING_WITHOUT_EQUALITY
    ) {
      // TODO: Verify validity of a vote to a question of type `QuestionTypeEnum.PREFERENTIAL_VOTING_WITHOUT_EQUALITY`. Is there a possibility for such a vote to be invalid? What could an invalid vote look like? What should we verify?
    } else if (questionType === QuestionTypeEnum.CLASSIC) {
      // Before moving on to next question, verify that user input respects question constraints:
      // - if blank vote is allowed on this question and user voted blank, then verify that no other answer is checked
      // - if this question accepts between X and Y answers and user has not voted blank, verify that user has not checked less than X answers, nor more than Y answers
      const number_of_answers_checked =
        voter_selected_answers_as_uncrypted_ballot[
          current_question_index
        ].reduce(function (accumulator, value, index) {
          const answer_value = value === 1 ? 1 : 0;
          return accumulator + answer_value;
        }, 0);
      const all_selected_answers_indexes = current_user_vote_for_all_questions[
        current_question_index
      ].reduce((accumulator, value, index) => {
        if (value === 1) {
          accumulator.push(index);
        }
        return accumulator;
      }, []);
      if (
        current_question_data.blankVoteIsAllowed === true &&
        voter_selected_answers_as_uncrypted_ballot[
          current_question_index
        ][0] === 1
      ) {
        if (number_of_answers_checked > 1) {
          dispatch_current_alerts_for_all_questions({
            type: "saveAlertForCandidateInQuestion",
            question_index: current_question_index,
            alert_id: "shouldEitherVoteBlankOrSomething",
            candidates_indexes: all_selected_answers_indexes,
            alert_text: t("alert_question_constraint_no_blank_and_other"),
          });
          user_vote_to_question_is_valid = false;
        }
      } else {
        if (number_of_answers_checked < current_question_data.min) {
          dispatch_current_alerts_for_all_questions({
            type: "saveAlertForCandidateInQuestion",
            question_index: current_question_index,
            alert_id: "shouldSelectMoreCandidates",
            candidates_indexes: undefined,
            alert_text: t("alert_question_constraint_no_less_than_min", {
              count: current_question_data.min,
            }),
          });
          user_vote_to_question_is_valid = false;
        }
        if (number_of_answers_checked > current_question_data.max) {
          dispatch_current_alerts_for_all_questions({
            type: "saveAlertForCandidateInQuestion",
            question_index: current_question_index,
            alert_id: "shouldSelectLessCandidates",
            candidates_indexes: all_selected_answers_indexes,
            alert_text: t("alert_question_constraint_no_more_than_max", {
              count: current_question_data.max,
            }),
          });
          user_vote_to_question_is_valid = false;
        }
      }
    }

    if (user_vote_to_question_is_valid) {
      dispatch_current_alerts_for_all_questions({
        type: "resetAllAlertsInQuestion",
        question_index: current_question_index,
      });
      if (current_question_index + 1 < props.electionObject.questions.length) {
        set_current_question_index(current_question_index + 1);
      } else {
        if (props.onVoteSubmit) {
          return props.onVoteSubmit(
            event,
            voter_selected_answers_as_uncrypted_ballot,
          );
        }
      }
    }
  };

  const renderedQuestions = props.electionObject.questions.map(
    function (question, question_index) {
      const identifierPrefix = `question_${question_index}_`;
      const visible = current_question_index === question_index ? true : false;
      return e(QuestionWithVotableAnswers, {
        question,
        identifierPrefix,
        visible,
        currentUserVoteForQuestion:
          current_user_vote_for_all_questions[question_index],
        currentAlertsTextsForQuestion: alertTextsInQuestion[question_index],
        currentCandidatesHavingAlertsForQuestion:
          candidatesIndexesHavingAlertInQuestion[question_index],
        dispatchUpdateUserVoteForQuestion:
          bindFunctionMergeObjectToFirstParameter(
            dispatch_current_user_vote_for_all_questions,
            { question_index: question_index },
          ),
      });
    },
  );

  const renderedPagination = e(VoteNavigation, {
    question_index: current_question_index,
    questions_length: props.electionObject.questions.length,
    onClickPreviousButton: onClickPrevious,
    onClickNextButton: onClickNext,
  });
  return e(React.Fragment, null, ...renderedQuestions, renderedPagination);
}

TranslatableAllQuestionsWithPagination.defaultProps = {
  current_question_index: 0,
  electionObject: null,
  onVoteSubmit: null,
  t: function (s) {
    return s;
  },
};

const AllQuestionsWithPagination = withTranslation()(
  TranslatableAllQuestionsWithPagination,
);

export {
  AllQuestionsWithPagination,
  TranslatableAllQuestionsWithPagination,
  QuestionTypeEnum,
  detectQuestionType,
};
export default AllQuestionsWithPagination;
