function TranslatableMajorityJudgmentVoteSmallCandidate({ candidateInfo, availableGrades, selectedGradeIndex=null, currentAlertsForCandidateInQuestion, dispatchUserVoteForCandidateInQuestion, availableGradesCssColors, t }){
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
  const bemBlockName = "majority-judgment-vote-small-candidate";
  let cssClasses = bemBlockName;
  if (currentAlertsForCandidateInQuestion){
    cssClasses += ` ${bemBlockName}--with-alert`;
  }
  return e(
    "div",
    {
      className: cssClasses,
      ...additionalPropsOnMain
    },
    e(
      "div",
      {
        className: `${bemBlockName}__candidate-info`
      },
      candidateInfo
    ),
    e(
      "select",
      {
        className: `${bemBlockName}__grade-selector select-css`,
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

function TranslatableMajorityJudgmentVoteSmallCandidatesList({ identifierPrefix, candidates, blankVoteIsAllowed, renderedBlankVoteComponent, availableGrades, currentUserVoteForQuestion, currentCandidatesHavingAlertsForQuestion, dispatchUpdateUserVoteForQuestion, availableGradesCssColors, t }){
  let renderedCandidates = candidates.map((candidate, candidateIndex) => {
    const identifier = `${identifierPrefix}_candidate_${candidateIndex}`;
    const dispatchUserVoteForCandidateInQuestion = (selected_grade) => {
      dispatchUpdateUserVoteForQuestion({
        type: 'saveVoteForCandidateInQuestion',
        candidate_index: candidateIndex,
        user_vote_for_candidate: selected_grade
      });
    }
    const currentAlerts = currentCandidatesHavingAlertsForQuestion && currentCandidatesHavingAlertsForQuestion.includes(candidateIndex);
    const commonProps = {
      candidateInfo: candidate,
      availableGrades,
      id: identifier,
      key: candidateIndex,
      selectedGradeIndex: currentUserVoteForQuestion[candidateIndex],
      currentAlertsForCandidateInQuestion: currentAlerts,
      dispatchUserVoteForCandidateInQuestion,
      availableGradesCssColors,
      t
    };
    return e(
      TranslatableMajorityJudgmentVoteSmallCandidate,
      commonProps
    );
  });
  if (renderedBlankVoteComponent){
    renderedCandidates.push(renderedBlankVoteComponent);
  }
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
