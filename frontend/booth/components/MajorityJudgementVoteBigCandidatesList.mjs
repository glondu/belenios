function MajorityJudgementVoteBigCandidateAvailableGrade({ name, id, checked=null, gradeLabel, availableGrades, gradeIndex, dispatchUserVoteForCandidateInQuestion, ...props }){
  const checkedValue = checked ? "checked" : null;
  const onChange = (event) => {
    if (event.target.checked){
      dispatchUserVoteForCandidateInQuestion(gradeIndex);
    }
  };
  return e(
    'div',
    {
      className: `majority-judgement-vote-big-candidate__grade clickable majority-judgement-vote-big-candidate__grade--not-selected majority-judgement-vote-big-candidate__grade--${gradeIndex}`, // TODO: `not-selected` should be dynamicly set
      ...props
    },
    e(
      'input',
      {
        type: 'radio',
        className: 'majority-judgement-vote-big-candidate__grade__input',
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
        className: 'majority-judgement-vote-big-candidate__grade__label'
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
          'className': 'majority-judgement-vote-big-candidate__grade__label__label'
        },
        gradeLabel
      )
    )
  );
}

MajorityJudgementVoteBigCandidateAvailableGrade.defaultProps = {
  name: "radio-button-choice",
  id: "radio-button_1",
  checked: false,
  gradeLabel: "Excellent",
  availableGrades: ["Poor", "Good", "Excellent"],
  gradeIndex: 2,
  dispatchUserVoteForCandidateInQuestion: (selected_grade_index) => {},
};

function TranslatableMajorityJudgementVoteBigCandidate({ candidateInfo, availableGrades, identifierPrefix, name, dispatchUserVoteForCandidateInQuestion, selectedGradeIndex, t }){
  const renderedGrades = availableGrades.map((gradeLabel, gradeIndex) => {
    const identifier = `${identifierPrefix}_grade_${gradeIndex}`;
    return e(
      MajorityJudgementVoteBigCandidateAvailableGrade,
      {
        name,
        id: identifier,
        checked: selectedGradeIndex === gradeIndex,
        gradeLabel,
        availableGrades,
        gradeIndex,
        dispatchUserVoteForCandidateInQuestion
      }
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
      e(
        "div",
        {
          className: "majority-judgement-vote-big-candidate__candidate-info__label"
        },
        candidateInfo
      )
    ),
    ...renderedGrades
  );
}

function TranslatableMajorityJudgementVoteBigCandidatesList({ identifierPrefix, candidates, availableGrades, currentUserVoteForQuestion, dispatchUpdateUserVoteForQuestion, t }){
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
      t
    };
    
    return e(
      TranslatableMajorityJudgementVoteBigCandidate,
      {
        ...commonProps
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
