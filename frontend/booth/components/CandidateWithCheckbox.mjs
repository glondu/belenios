function CandidateWithCheckbox({ name, id, checked, candidateInfo, dispatchUpdateUserVoteForCandidateInQuestion, currentAlertsForCandidateInQuestion, ...props }){
  const checkedValue = checked ? true : false;
  const onChange = (event) => {
    dispatchUpdateUserVoteForCandidateInQuestion(event.target.checked === true ? true : false);
  };
  let cssClasses = "candidate-with-checkbox clickable";
  if (currentAlertsForCandidateInQuestion){
    cssClasses += ' candidate-with-checkbox--with-alert';
  }
  return e(
    'div',
    {
      className: cssClasses,
      ...props
    },
    e(
      'input',
      {
        type: 'checkbox',
        name: name,
        id: id,
        defaultChecked: checkedValue,
        onChange: onChange
      }
    ),
    e(
      'label',
      {
        htmlFor: id
      },
      e(
        'span',
        {
          'className': 'checkbox-appearance'
        }
      ),
      e(
        'span',
        {
          'className': 'candidate-info'
        },
        candidateInfo
      )
    )
  );
}

CandidateWithCheckbox.defaultProps = {
  name: "radio-button-choice",
  id: "checkbox_1",
  checked: false,
  candidateInfo: "choice 1",
  dispatchUpdateUserVoteForCandidateInQuestion: () => {},
  currentAlertsForCandidateInQuestion: undefined
};

export { CandidateWithCheckbox };
export default CandidateWithCheckbox;
