import i18n_init from "./i18n_init.mjs";
import PageHeader from "./components/PageHeader.mjs";
import { VoteBreadcrumb } from "./components/Breadcrumb.mjs";
import AllQuestionsWithPagination from "./components/AllQuestionsWithPagination.mjs";
import NoUuidSection from "./components/NoUuidSection.mjs";
import InputCredentialSection from "./components/InputCredentialSection.mjs";
import ReviewEncryptSection from "./components/ReviewEncryptSection.mjs";
import { PageFooter, EmptyPageFooter } from "./components/PageFooter.mjs";

const relativeServerRootFolder = "../../..";

function getHashParametersFromURL(){
  const url_hash_parameters = window.location.hash.substr(1);
  return url_hash_parameters.split('&').reduce(function (result, item) {
    const parts = item.split('=');
    result[parts[0]] = parts[1];
    return result;
  }, {});
}

function VotePage({ electionData, electionFingerprint, currentStep, children }){
  return e(
    "div",
    {
      className: "page"
    },
    e(
      PageHeader,
      {
        title: electionData.name,
        subTitle: electionData.description
      }
    ),
    e(
      "div",
      {
        className: "page-body"
      },
      e(
        VoteBreadcrumb,
        {
          currentStep: currentStep
        }
      ),
      children
    ),
    e(
      PageFooter,
      {
        electionUuid: electionData.uuid,
        electionFingerprint: electionFingerprint
      }
    )
  );
}

function GenericPage({title=null, subTitle=null, children}){
  return e(
    "div",
    {
      className: "page"
    },
    e(
      PageHeader,
      {
        title: title,
        subTitle: subTitle
      }
    ),
    e(
      "div",
      {
        className: "page-body"
      },
      children
    ),
    e(
      EmptyPageFooter
    )
  );
}

function TranslatableVoteApp({uuid=null, t}){
  const [currentStep, setCurrentStep] = React.useState(1);
  const [electionData, setElectionData] = React.useState({});
  const [electionFingerprint, setElectionFingerprint] = React.useState("");
  const [credential, setCredential] = React.useState(null);
  const [electionLoadingStatus, setElectionLoadingStatus] = React.useState(0); // 0: not yet loaded. 1: loaded with success. 2: loaded with error.
  const [uncryptedBallotBeforeReview, setUncryptedBallotBeforeReview] = React.useState(null);
  const [cryptedBallotBeforeReview, setCryptedBallotBeforeReview] = React.useState(null);
  const [smartBallotTracker, setSmartBallotTracker] = React.useState(null);

  const processElectionData = (inputElectionData) => {
    setElectionData(inputElectionData);
    setElectionFingerprint(belenios.computeFingerprint(inputElectionData));
    setElectionLoadingStatus(1);
  };

  const loadElectionDataFromUuid = (uuid) => {
    fetch(`${relativeServerRootFolder}/elections/${uuid}/election.json`)
      .then(response => {
        if(!response.ok){
          return fetch(`${relativeServerRootFolder}/draft/preview/${uuid}/election.json`);
        }
        return response;
      })
      .then(response => {
        if(!response.ok){
          setElectionLoadingStatus(2);
        }
        else {
          response.json().then(processElectionData);
        }
      });
  };
  
  React.useEffect(() => {
    if(uuid){
      loadElectionDataFromUuid(uuid);
    }
  }, []);


  if(!uuid && electionLoadingStatus == 0){
    const onClickLoadFromParameters = (election_params) => {
      let inputElectionData = null;
      try {
        inputElectionData = JSON.parse(election_params);
      } catch (e) {
        alert(`Election parameters seem to be invalid. Parsing error: ${e}`); 
      }
      processElectionData(inputElectionData);
    };

    const onClickLoadFromUuid = (uuid) => {
      // v1:
      // document.location.href = `#uuid=${uuid}`;
      // document.location.reload();

      // v2:
      loadElectionDataFromUuid(uuid);
    };

    const titleMessage = t("Belenios booth");
    
    return e(
      GenericPage,
      {
        title: titleMessage,
        subTitle: null
      },
      e(
        NoUuidSection,
        {
          onClickLoadFromUuid: onClickLoadFromUuid,
          onClickLoadFromParameters: onClickLoadFromParameters
        }
      )
    );
  }
  else if(electionLoadingStatus === 0 || electionLoadingStatus === 2){
    const titleMessage = electionLoadingStatus === 0 ? "Loading..." : "Error";
    const loadingMessage = electionLoadingStatus === 0 ? titleMessage : "Error: Could not load this election. Maybe no election exists with this identifier.";

    return e(
      GenericPage,
      {
        title: titleMessage,
        subTitle: null
      },
      e(
        "div",
        {
          style: {
            textAlign: "center",
            padding: "30px 0"
          }
        },
        loadingMessage
      )
    );
  }
  else {
    if (currentStep === 1){
      return e(
        VotePage,
        {
          electionData: electionData,
          electionFingerprint: electionFingerprint,
          currentStep: currentStep
        },
        e(
          InputCredentialSection,
          {
            onSubmit: function(credential){
              if(belenios.checkCredential(credential) === true){
                setCredential(credential);
                setCurrentStep(2);
              }
              else {
                alert(t("Invalid credential!"));
              }
              return false;
            }
          }
        )
      );
    }
    else if (currentStep === 2) {
      return e(
        VotePage,
        {
          electionData: electionData,
          electionFingerprint: electionFingerprint,
          currentStep: currentStep
        },
        e(
          AllQuestionsWithPagination,
          {
            electionData: electionData,
            extractVoterSelectedAnswersFromFields: extractVoterSelectedAnswersFromFields,
            onVoteSubmit: async function(event, electionData){
              const voter_selected_answers = extractVoterSelectedAnswersFromFields(electionData);
              setUncryptedBallotBeforeReview(voter_selected_answers);
              setCryptedBallotBeforeReview(null);
              setSmartBallotTracker(null);
              setCurrentStep(3);
              const encryptBallotSuccessCallback = (ballot, tracker) => {
                console.log("Raw encrypted ballot:", JSON.parse(ballot));
                console.log("Smart ballot tracker:", tracker);
                setCryptedBallotBeforeReview(ballot);
                setSmartBallotTracker(tracker);
              };
              const encryptBallotErrorCallback = (error) => {
                alert("Error: " + error);
              };
              belenios.encryptBallot(
                electionData, credential, voter_selected_answers,
                encryptBallotSuccessCallback, encryptBallotErrorCallback
              );
            }
          }
        )
      );
    }
    else if (currentStep === 3) {
      const urlToPostEncryptedBallot = `${relativeServerRootFolder}/election/submit-ballot`;
      const onClickGiveUp = () => {
        setCurrentStep(1);
        setCredential(null);
        setUncryptedBallotBeforeReview(null);
        setCryptedBallotBeforeReview(null);
        setSmartBallotTracker(null);
      };
      return e(
        VotePage,
        {
          electionData: electionData,
          electionFingerprint: electionFingerprint,
          currentStep: currentStep
        },
        e(
          ReviewEncryptSection,
          {
            electionData: electionData,
            uncryptedBallot: uncryptedBallotBeforeReview,
            cryptedBallot: cryptedBallotBeforeReview,
            smartBallotTracker: smartBallotTracker,
            onClickGiveUp: onClickGiveUp,
            urlToPostEncryptedBallot: urlToPostEncryptedBallot
          }
        )
      );
    }
  }
}

const VoteApp = ReactI18next.withTranslation()(TranslatableVoteApp);

const afterI18nInitialized = (uuid, lang) => {
  return function(){
    document.title = i18next.t("Belenios booth");
    document.querySelector("html").setAttribute("lang", 
i18next.languages[0] || "en");
    const container = document.querySelector("#vote-app");
    ReactDOM.render(
      e(
        VoteApp,
        {
          uuid: uuid
        }
      ),
      container
    );
  }
};

function main() {
  const hash_parameters = getHashParametersFromURL();
  const lang = hash_parameters['lang'];
  const uuid = hash_parameters['uuid'];
  const container = document.querySelector("#vote-app");
  container.innerHTML = "Loading...";
  i18n_init(lang, afterI18nInitialized(uuid, lang));
}

function extractVoterSelectedAnswersFromFields(electionData){
  const question_x_choice_y_pattern = (question_index, answer_index) => `question_${question_index}__choice_${answer_index}`;
  let vote_of_voter_per_question = []; // array where each element correspond to voter's vote on question i. if type of question i is checkbox, answer to this question is an array of indexes of answers. if type of question i is radio, answer to this question is an array containing only one index of answer.
  vote_of_voter_per_question = electionData.questions.map(function(question, question_index){
    const question_type = question.min === 1 && question.max === 1 ? "radio" : "checkbox";
    const els = question.answers.map(
      function(v, index){
        return document.querySelector("#" + question_x_choice_y_pattern(question_index, index));
      }
    );
    // attribute `checked`` works for questions of type `<input type="checkbox">` as well as `<input type="radio">`
    let answers_to_question = els.map(el => el.checked).reduce(
      function(accumulator, value, index){
        const answer_value = value === true ? 1 : 0;
        accumulator.push(answer_value);
        return accumulator;
      },
      []
    );
    if ("blank" in question && question["blank"] === true){
      const blank_el = document.querySelector("#" + question_x_choice_y_pattern(question_index, question.answers.length));
      const blank_value = blank_el.checked ? 1 : 0;
      answers_to_question = [blank_value, ...answers_to_question];
    }
    return answers_to_question;
  });
  return vote_of_voter_per_question;
}

main();
