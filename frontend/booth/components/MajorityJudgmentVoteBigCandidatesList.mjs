function MajorityJudgmentVoteBigCandidateAvailableGrade({ name, id, checked=null, gradeLabel, availableGrades, gradeIndex, dispatchUserVoteForCandidateInQuestion, availableGradesCssColors, ...props }){
  const checkedValue = checked ? "checked" : null;
  const onChange = (event) => {
    if (event.target.checked){
      dispatchUserVoteForCandidateInQuestion(gradeIndex);
    }
  };
  return e(
    'div',
    {
      className: `majority-judgment-vote-big-candidate__grade clickable`,
      style: {
        '--majority-judgment-available-grade-color': availableGradesCssColors[gradeIndex]
      },
      ...props
    },
    e(
      'input',
      {
        type: 'radio',
        className: 'majority-judgment-vote-big-candidate__grade__input',
        name: name,
        id: id,
        value: gradeIndex,
        defaultChecked: checkedValue,
        onChange: onChange
      }
    ),
    e(
      'label',
      {
        htmlFor: id,
        className: 'majority-judgment-vote-big-candidate__grade__label'
      },
      e(
        'span',
        {
          'className': 'radio-button-appearance'
        }
      ),
      e(
        'span',
        {
          'className': 'majority-judgment-vote-big-candidate__grade__label__label'
        },
        gradeLabel
      )
    )
  );
}

MajorityJudgmentVoteBigCandidateAvailableGrade.defaultProps = {
  name: "radio-button-choice",
  id: "radio-button_1",
  checked: false,
  gradeLabel: "Excellent",
  availableGrades: ["Poor", "Good", "Excellent"],
  gradeIndex: 2,
  dispatchUserVoteForCandidateInQuestion: (selected_grade_index) => {},
  availableGradesCssColors: ["red", "yellow", "green"]
};

function TranslatableMajorityJudgmentVoteBigCandidate({ candidateInfo, availableGrades, identifierPrefix, name, dispatchUserVoteForCandidateInQuestion, selectedGradeIndex, availableGradesCssColors, t }){
  const renderedGrades = availableGrades.map((gradeLabel, gradeIndex) => {
    const identifier = `${identifierPrefix}_grade_${gradeIndex}`;
    return e(
      MajorityJudgmentVoteBigCandidateAvailableGrade,
      {
        name,
        id: identifier,
        checked: selectedGradeIndex === gradeIndex,
        gradeLabel,
        availableGrades,
        gradeIndex,
        dispatchUserVoteForCandidateInQuestion,
        availableGradesCssColors
      }
    );
  });
  return e(
    "div",
    {
      className: "majority-judgment-vote-big-candidate"
    },
    e(
      "div",
      {
        className: "majority-judgment-vote-big-candidate__candidate-info"
      },
      e(
        "div",
        {
          className: "majority-judgment-vote-big-candidate__candidate-info__label"
        },
        candidateInfo
      )
    ),
    ...renderedGrades
  );
}

function TranslatableMajorityJudgmentVoteBigCandidatesList({ identifierPrefix, candidates, availableGrades, currentUserVoteForQuestion, dispatchUpdateUserVoteForQuestion, availableGradesCssColors, t }){
  const renderedCandidates = candidates.map((candidate, candidateIndex) => {
    const identifierConsolidatedPrefix = `${identifierPrefix}_candidate_${candidateIndex}`;
    const dispatchUserVoteForCandidateInQuestion = (selected_grade_index) => {
      dispatchUpdateUserVoteForQuestion({
        type: 'saveVoteForCandidateInQuestion',
        candidate_index: candidateIndex,
        user_vote_for_candidate: selected_grade_index
      });
    };
    const commonProps = {
      candidateInfo: candidate,
      availableGrades,
      key: candidateIndex,
      identifierPrefix: identifierConsolidatedPrefix,
      name: identifierConsolidatedPrefix,
      dispatchUserVoteForCandidateInQuestion,
      selectedGradeIndex: currentUserVoteForQuestion[candidateIndex],
      availableGradesCssColors,
      t
    };
    
    return e(
      TranslatableMajorityJudgmentVoteBigCandidate,
      {
        ...commonProps
      }
    );
  });

  return e(
    'div',
    {
      className: "majority-judgment-vote-big-candidates-list noselect"
    },
    ...renderedCandidates
  );
}

export { TranslatableMajorityJudgmentVoteBigCandidatesList };
export default TranslatableMajorityJudgmentVoteBigCandidatesList;
