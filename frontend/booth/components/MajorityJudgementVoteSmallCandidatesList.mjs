function TranslatableMajorityJudgementVoteSmallCandidate({ candidateInfo, availableGrades, selectedGradeIndex=null, t }){
  const renderedAvailableGrades = availableGrades.map((availableGrade, index) => {
    const isSelected = index === selectedGradeIndex ? true : false;
    return e(
      "option",
      {
        value: index,
        selected: isSelected
      },
      availableGrade
    );
  });
  const onChange = (event) => {
    event.target.dataset.value = event.target.value;
  };
  let additionalProps = {};
  if (selectedGradeIndex !== null && selectedGradeIndex !== undefined){
    additionalProps['data-value'] = selectedGradeIndex
  }
  return e(
    "div",
    {
      className: "majority-judgement-vote-small-candidate"
    },
    e(
      "div",
      {
        className: "majority-judgement-vote-small-candidate__candidate-info"
      },
      candidateInfo
    ),
    e(
      "select",
      {
        className: "majority-judgement-vote-small-candidate__grade-selector select-css",
        onChange: onChange,
        ...additionalProps
      },
      e(
        "option",
        {
          value: ""
        },
        t("Please select an option")
      ),
      ...renderedAvailableGrades
    )
  );
}

function TranslatableMajorityJudgementVoteSmallCandidatesList({ identifierPrefix, candidates, availableGrades, t }){
  const renderedCandidates = candidates.map((candidate, instanceNumber) => {
    const identifier = `${identifierPrefix}_candidate_${instanceNumber}`;
    const commonProps = {
      candidateInfo: candidate,
      availableGrades: availableGrades,
      id: identifier,
      key: instanceNumber,
      t: t
    };
    return e(
      TranslatableMajorityJudgementVoteSmallCandidate,
      commonProps
    );
  });
  return e(
    "div",
    {
      className: "majority-judgement-vote-small-candidates-list"
    },
    ...renderedCandidates
  );
}

export { TranslatableMajorityJudgementVoteSmallCandidatesList };
export default TranslatableMajorityJudgementVoteSmallCandidatesList;
