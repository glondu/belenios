function TranslatableMajorityJudgementVoteSmallCandidate({ candidateInfo, availableGrades, selectedGradeIndex=null, dispatchUserVoteForCandidateInQuestion, t }){
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
    const val = event.target.value
    event.target.dataset.value = val;
    const valAsInt = parseInt(val, 10);
    const finalVal = isNaN(valAsInt) ? undefined : valAsInt;
    dispatchUserVoteForCandidateInQuestion(finalVal);
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

function TranslatableMajorityJudgementVoteSmallCandidatesList({ identifierPrefix, candidates, availableGrades, currentUserVoteForQuestion, dispatchUpdateUserVoteForQuestion, t }){
  const renderedCandidates = candidates.map((candidate, candidateIndex) => {
    const identifier = `${identifierPrefix}_candidate_${candidateIndex}`;
    const dispatchUserVoteForCandidateInQuestion = (selected_grade) => {
      dispatchUpdateUserVoteForQuestion({
        type: 'saveVoteForCandidateInQuestion',
        candidate_index: candidateIndex,
        user_vote_for_candidate: selected_grade
      });
    }
    const commonProps = {
      candidateInfo: candidate,
      availableGrades,
      id: identifier,
      key: candidateIndex,
      selectedGradeIndex: currentUserVoteForQuestion[candidateIndex],
      dispatchUserVoteForCandidateInQuestion,
      t
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
