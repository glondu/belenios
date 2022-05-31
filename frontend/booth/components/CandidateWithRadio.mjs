function CandidateWithRadio({ name, id, value, checked, candidateInfo, dispatchUpdateUserVoteForCandidateInQuestion, currentAlertsForCandidateInQuestion, ...props }){
  const checkedValue = checked ? "checked" : null;
  const onChange = (event) => {
    dispatchUpdateUserVoteForCandidateInQuestion(event.target.checked === true ? true : false)
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
        type: 'radio',
        name: name,
        id: id,
        value: value,
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
          'className': 'radio-button-appearance'
        }
      ),
      e(
        'span',
        {
          'className': 'candidate-info'
        },
        txt_br(candidateInfo)
      )
    )
  );
}

CandidateWithRadio.defaultProps = {
  name: "radio-button-choice",
  id: "radio-button_1",
  value: "choice_1",
  checked: false,
  candidateInfo: "choice 1",
  dispatchUpdateUserVoteForCandidateInQuestion: () => {},
  currentAlertsForCandidateInQuestion: undefined
};

export { CandidateWithRadio };
export default CandidateWithRadio;
