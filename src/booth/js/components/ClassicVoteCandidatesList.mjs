import CandidateWithCheckbox from "./CandidateWithCheckbox.mjs";
import CandidateWithRadio from "./CandidateWithRadio.mjs";

/*
Displays a list of candidates represented using instances of component CandidateWithCheckbox or CandidateWithRadio, depending on value of "type" prop.
*/
class TranslatableClassicVoteCandidatesList extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { type, candidates, identifierPrefix, blankVoteAllowed, t } = this.props;
    const candidate_constructor = type == "checkbox" ? CandidateWithCheckbox : CandidateWithRadio;
    let finalCandidates = candidates;
    if (blankVoteAllowed === true){
      const blankVoteLabel = t("Blank vote");
      finalCandidates = [...candidates, blankVoteLabel]; // Is this the right way to do it?
    }
    const renderedCandidates = finalCandidates.map((candidate, instanceNumber) => {
      const identifier = `${identifierPrefix}_choice_${instanceNumber}`;
      const commonProps = {
        candidateInfo: candidate,
        checked: false,
        id: identifier,
        key: instanceNumber
      };
      const additionalProps = type == "checkbox" ? {
        name: identifier
      } : { 
        name: identifierPrefix,
        value: `choice_${instanceNumber}` // or maybe a candidate id provided in data input, or slugification of candidate name?
      };
      let blankVoteProps = {};
      if (blankVoteAllowed === true && instanceNumber === candidates.length){
        blankVoteProps = {
          style: {marginTop: "30px"}
        };
      }
      return e(
        candidate_constructor,
        {
          ...commonProps,
          ...additionalProps,
          ...blankVoteProps
        }
      );
    });

    return e(
      'div',
      {
        className: "classic-vote-candidates-list noselect"
      },
      ...renderedCandidates
    );
  }
}

TranslatableClassicVoteCandidatesList.defaultProps = {
  type: "checkbox",
  identifierPrefix: "question_1",
  candidates: [
    "Answer 1",
    "Answer 2",
    "Answer 3"
  ],
  blankVoteAllowed: false
};

const ClassicVoteCandidatesList = ReactI18next.withTranslation()(TranslatableClassicVoteCandidatesList);

export { ClassicVoteCandidatesList, TranslatableClassicVoteCandidatesList };
export default ClassicVoteCandidatesList;
