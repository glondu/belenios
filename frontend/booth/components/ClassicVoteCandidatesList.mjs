import CandidateWithCheckbox from "./CandidateWithCheckbox.mjs";
import CandidateWithRadio from "./CandidateWithRadio.mjs";

/*
Displays a list of candidates represented using instances of component CandidateWithCheckbox or CandidateWithRadio, depending on value of "type" prop.
*/
function TranslatableClassicVoteCandidatesList({ type, candidates, identifierPrefix, blankVoteAllowed, currentUserVoteForQuestion, currentCandidatesHavingAlertsForQuestion, dispatchUpdateUserVoteForQuestion, t }) {
  const candidate_constructor = type == "checkbox" ? CandidateWithCheckbox : CandidateWithRadio;
  let finalCandidates = candidates;
  if (blankVoteAllowed === true){
    const blankVoteLabel = t("Blank vote");
    finalCandidates = [...candidates, blankVoteLabel]; // Is this the right way to do it?
  }
  const renderedCandidates = finalCandidates.map((candidate, candidateIndex) => {
    const identifier = `${identifierPrefix}_choice_${candidateIndex}`;
    let dispatchUpdateUserVoteForCandidateInQuestion = null;
    if (type == "checkbox"){
      dispatchUpdateUserVoteForCandidateInQuestion = (candidate_is_selected) => {
        dispatchUpdateUserVoteForQuestion({
          type: 'saveVoteForCandidateInQuestion',
          candidate_index: candidateIndex,
          user_vote_for_candidate: candidate_is_selected === true ? 1 : 0
        });
      };
    }
    else {
      dispatchUpdateUserVoteForCandidateInQuestion = (candidate_is_selected) => {
        dispatchUpdateUserVoteForQuestion({
          type: candidate_is_selected === true ? 'saveVoteForCandidateInQuestionAndResetOthers' : 'saveVoteForCandidateInQuestion',
          candidate_index: candidateIndex,
          user_vote_for_candidate: candidate_is_selected === true ? 1 : 0
        });
      };
    }
    const currentAlert = currentCandidatesHavingAlertsForQuestion && currentCandidatesHavingAlertsForQuestion.includes(candidateIndex);
    const commonProps = {
      candidateInfo: candidate,
      checked: currentUserVoteForQuestion[candidateIndex] === 1 ? true : false,
      id: identifier,
      key: candidateIndex,
      dispatchUpdateUserVoteForCandidateInQuestion,
      currentAlertsForCandidateInQuestion: currentAlert
    };
    const additionalProps = type == "checkbox" ? {
      name: identifier
    } : { 
      name: identifierPrefix,
      value: `choice_${candidateIndex}` // or maybe a candidate id provided in data input, or slugification of candidate name?
    };
    let blankVoteProps = {};
    if (blankVoteAllowed === true && candidateIndex === candidates.length){
      blankVoteProps = {
        style: {marginTop: "30px"}
      };
    }
    return e(
      candidate_constructor,
      {
        ...commonProps,
        ...additionalProps,
        ...blankVoteProps
      }
    );
  });

  return e(
    'div',
    {
      className: "classic-vote-candidates-list noselect"
    },
    ...renderedCandidates
  );
}

TranslatableClassicVoteCandidatesList.defaultProps = {
  type: "checkbox",
  identifierPrefix: "question_1",
  candidates: [
    "Answer 1",
    "Answer 2",
    "Answer 3"
  ],
  blankVoteAllowed: false,
  currentUserVoteForQuestion: [],
  currentCandidatesHavingAlertsForQuestion: [],
  dispatchUpdateUserVoteForQuestion: () => {},
  t: (s) => {return s;}
};

const ClassicVoteCandidatesList = ReactI18next.withTranslation()(TranslatableClassicVoteCandidatesList);

export { ClassicVoteCandidatesList, TranslatableClassicVoteCandidatesList };
export default ClassicVoteCandidatesList;
