function TranslatableMajorityJudgmentVoteSmallCandidate({ candidateInfo, availableGrades, selectedGradeIndex=null, dispatchUserVoteForCandidateInQuestion, availableGradesCssColors, t }){
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
  let additionalPropsOnSelect = {};
  let additionalPropsOnMain = {};
  if (selectedGradeIndex !== null && selectedGradeIndex !== undefined){
    additionalPropsOnSelect['data-value'] = selectedGradeIndex;
    additionalPropsOnMain['style'] = {
      "--selected-grade-color": availableGradesCssColors[selectedGradeIndex]
    }
  }
  return e(
    "div",
    {
      className: "majority-judgment-vote-small-candidate",
      ...additionalPropsOnMain
    },
    e(
      "div",
      {
        className: "majority-judgment-vote-small-candidate__candidate-info"
      },
      candidateInfo
    ),
    e(
      "select",
      {
        className: "majority-judgment-vote-small-candidate__grade-selector select-css",
        onChange: onChange,
        ...additionalPropsOnSelect
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

function TranslatableMajorityJudgmentVoteSmallCandidatesList({ identifierPrefix, candidates, blankVoteAllowed, availableGrades, currentUserVoteForQuestion, dispatchUpdateUserVoteForQuestion, availableGradesCssColors, t }){
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
      availableGradesCssColors,
      t
    };
    return e(
      TranslatableMajorityJudgmentVoteSmallCandidate,
      commonProps
    );
  });
  return e(
    "div",
    {
      className: "majority-judgment-vote-small-candidates-list"
    },
    ...renderedCandidates
  );
}

export { TranslatableMajorityJudgmentVoteSmallCandidatesList };
export default TranslatableMajorityJudgmentVoteSmallCandidatesList;
