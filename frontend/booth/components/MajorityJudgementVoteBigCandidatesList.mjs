function MajorityJudgementVoteBigCandidateAvailableGrade({ name, id, value, checked, gradeLabel, availableGrades, gradeIndex, ...props }){
  const checkedValue = checked ? "checked" : null;
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
        value: value,
        defaultChecked: checkedValue
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
  value: "choice_1",
  checked: false,
  gradeLabel: "Excellent",
  availableGrades: ["Poor", "Good", "Excellent"],
  gradeIndex: 2
};

function TranslatableMajorityJudgementVoteBigCandidate({ candidateInfo, availableGrades, identifierPrefix, name, t }){
  const renderedGrades = availableGrades.map((gradeLabel, gradeIndex) => {
    const identifier = `${identifierPrefix}_grade_${gradeIndex}`;
    return e(
      MajorityJudgementVoteBigCandidateAvailableGrade,
      {
        name: name,
        id: identifier,
        value: "aaa", // TODO
        checked: null,
        gradeLabel,
        availableGrades,
        gradeIndex
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

function TranslatableMajorityJudgementVoteBigCandidatesList({ identifierPrefix, candidates, availableGrades, t }){
  const renderedCandidates = candidates.map((candidate, instanceNumber) => {
    const identifierConsolidatedPrefix = `${identifierPrefix}_candidate_${instanceNumber}`;
    const commonProps = {
      candidateInfo: candidate,
      availableGrades: availableGrades,
      id: identifierConsolidatedPrefix,
      key: instanceNumber,
      identifierPrefix: identifierConsolidatedPrefix,
      name: identifierConsolidatedPrefix,
      t: t
    };
    
    return e(
      TranslatableMajorityJudgementVoteBigCandidate,
      commonProps
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
