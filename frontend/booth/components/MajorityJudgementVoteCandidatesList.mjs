import DisplayDependingOnWindowWidth from "./DisplayDependingOnWindowWidth.mjs";
import { TranslatableMajorityJudgementVoteSmallCandidatesList } from "./MajorityJudgementVoteSmallCandidatesList.mjs";
import { TranslatableMajorityJudgementVoteBigCandidatesList } from "./MajorityJudgementVoteBigCandidatesList.mjs";

function TranslatableMajorityJudgementVoteCandidatesList({ identifierPrefix, availableGrades, candidates, t }){
  return e(
    "div",
    {
      className: "majority-judgement-vote-candidates-list noselect"
    },
    e(
      DisplayDependingOnWindowWidth,
      {
        widthLimit: 800,
        smallComponent: TranslatableMajorityJudgementVoteSmallCandidatesList,
        bigComponent: TranslatableMajorityJudgementVoteBigCandidatesList,
        identifierPrefix: identifierPrefix,
        candidates: candidates,
        availableGrades: availableGrades,
        t: t
      }
    )
  );
}

TranslatableMajorityJudgementVoteCandidatesList.defaultProps = {
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
  t: function(s){ return s; }
};

const MajorityJudgementVoteCandidatesList = ReactI18next.withTranslation()(TranslatableMajorityJudgementVoteCandidatesList);

export { MajorityJudgementVoteCandidatesList, TranslatableMajorityJudgementVoteCandidatesList };
export default MajorityJudgementVoteCandidatesList;
