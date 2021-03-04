import DisplayDependingOnWindowWidth from "./DisplayDependingOnWindowWidth.mjs";
import { TranslatableMajorityJudgmentVoteSmallCandidatesList } from "./MajorityJudgmentVoteSmallCandidatesList.mjs";
import { TranslatableMajorityJudgmentVoteBigCandidatesList } from "./MajorityJudgmentVoteBigCandidatesList.mjs";
import { majorityJudgmentGradeIndexToCssColor } from "../majority_judgment_colors.mjs";

function TranslatableMajorityJudgmentVoteCandidatesList({ identifierPrefix, availableGrades, candidates, blankVoteAllowed, currentUserVoteForQuestion, dispatchUpdateUserVoteForQuestion, t }){
  const availableGradesCssColors = React.useMemo(() => {
    return availableGrades.map((grade, index) => {
      return majorityJudgmentGradeIndexToCssColor(availableGrades.length, index);
    })
  }, availableGrades);
  return e(
    "div",
    {
      className: "majority-judgment-vote-candidates-list noselect"
    },
    e(
      DisplayDependingOnWindowWidth,
      {
        widthLimit: 800,
        smallComponent: TranslatableMajorityJudgmentVoteSmallCandidatesList,
        bigComponent: TranslatableMajorityJudgmentVoteBigCandidatesList,
        identifierPrefix,
        candidates,
        blankVoteAllowed,
        availableGrades,
        currentUserVoteForQuestion,
        dispatchUpdateUserVoteForQuestion,
        availableGradesCssColors,
        t
      }
    )
  );
}

TranslatableMajorityJudgmentVoteCandidatesList.defaultProps = {
  identifierPrefix: "question_1",
  availableGrades: [
    "Poor",
    "Good",
    "Excellent"
  ],
  candidates: [
    "Candidate 1",
    "Candidate 2",
    "Candidate 3"
  ],
  blankVoteAllowed: false,
  t: function(s){ return s; },
  currentUserVoteForQuestion: [],
  dispatchUpdateUserVote: () => {}
};

const MajorityJudgmentVoteCandidatesList = ReactI18next.withTranslation()(TranslatableMajorityJudgmentVoteCandidatesList);

export { MajorityJudgmentVoteCandidatesList, TranslatableMajorityJudgmentVoteCandidatesList };
export default MajorityJudgmentVoteCandidatesList;
