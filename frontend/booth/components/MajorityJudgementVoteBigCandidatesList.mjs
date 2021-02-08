function TranslatableMajorityJudgementVoteBigCandidate({ candidateInfo, availableGrades, t }){
  const renderedGrades = availableGrades.map((gradeLabel, index) => {
    return e(
      "div",
      {
        className: `majority-judgement-vote-big-candidate__grade majority-judgement-vote-big-candidate__grade--not-selected majority-judgement-vote-big-candidate__grade--${index}`
      },
      gradeLabel
    );
  });
  return e(
    "div",
    {
      className: "majority-judgement-vote-big-candidate"
    },
    e(
      "div",
      {
        className: "majority-judgement-vote-big-candidate__candidate-info"
      },
      candidateInfo
    ),
    ...renderedGrades
  );
}

function TranslatableMajorityJudgementVoteBigCandidatesList({ identifierPrefix, candidates, availableGrades, t }){

  const renderedCandidates = candidates.map((candidate, instanceNumber) => {
    const identifier = `${identifierPrefix}_choice_${instanceNumber}`;
    const commonProps = {
      candidateInfo: candidate,
      availableGrades: availableGrades,
      id: identifier,
      key: instanceNumber
    };
    const additionalProps = { 
      name: identifierPrefix,
      value: `choice_${instanceNumber}` // or maybe a candidate id provided in data input, or slugification of candidate name?
    };
    
    return e(
      TranslatableMajorityJudgementVoteBigCandidate,
      {
        ...commonProps,
        ...additionalProps,
        t
      }
    );
  });

  return e(
    'div',
    {
      className: "majority-judgement-vote-big-candidates-list noselect"
    },
    ...renderedCandidates
  );
}

export { TranslatableMajorityJudgementVoteBigCandidatesList };
export default TranslatableMajorityJudgementVoteBigCandidatesList;
