import ReactDOM from "react-dom/client";
import React, { createElement as e } from "react";
import i18next from "i18next";
import { withTranslation } from "react-i18next";

import i18n_init from "./i18n_init.mjs";
import PageHeader from "./components/PageHeader.mjs";
import { VoteBreadcrumb } from "./components/Breadcrumb.mjs";
import { AllQuestionsWithPagination } from "./components/AllQuestionsWithPagination.mjs";
import NoUuidSection from "./components/NoUuidSection.mjs";
import InputCredentialSection from "./components/InputCredentialSection.mjs";
import ReviewEncryptSection from "./components/ReviewEncryptSection.mjs";
import { PageFooter, EmptyPageFooter } from "./components/PageFooter.mjs";
import { Election } from "./election_utils.mjs";

const relativeServerRootFolder = "../../..";

function getHashParametersFromURL(){
  const url_hash_parameters = window.location.hash.substr(1);
  return url_hash_parameters.split('&').reduce(function (result, item) {
    const parts = item.split('=');
    result[parts[0]] = parts[1];
    return result;
  }, {});
}

function VotePage({ electionObject, electionFingerprint, currentStep, children }){
  return e(
    "div",
    {
      className: "page"
    },
    e(
      PageHeader,
      {
        title: electionObject.title,
        subTitle: electionObject.description
      }
    ),
    e(
      "div",
      {
        className: "page-body",
        id: "main" // used to ease targetting of DOM elements in automated tests
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
        electionUuid: electionObject.uuid,
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

function TranslatableVoteApp({uuid=null, votingCredential=null, draft, t}){
  const [currentStep, setCurrentStep] = React.useState(votingCredential ? 2 : 1); // Current step of the workflow displayed in the Breadcrumb. 1: input credential. 2: answer questions. 3: review and encrypt.
  const [electionData, setElectionData] = React.useState({});
  const [electionObject, setElectionObject] = React.useState(undefined);
  const [electionFingerprint, setElectionFingerprint] = React.useState("");
  const [credential, setCredential] = React.useState(votingCredential);
  const [electionLoadingStatus, setElectionLoadingStatus] = React.useState(0); // 0: not yet loaded. 1: loaded with success. 2: loaded with error.
  const [electionLoadingErrorMessage, setElectionLoadingErrorMessage] = React.useState(null);
  const [uncryptedBallotBeforeReview, setUncryptedBallotBeforeReview] = React.useState(null);
  const [cryptedBallotBeforeReview, setCryptedBallotBeforeReview] = React.useState(null);
  const [smartBallotTracker, setSmartBallotTracker] = React.useState(null);

  const processElectionData = (inputElectionData) => {
    setElectionData(inputElectionData);
    try {
      let election = new Election(inputElectionData);
      setElectionObject(election);
    }
    catch (error){
      setElectionLoadingErrorMessage(error);
      setElectionLoadingStatus(2);
      return;
    }
    setElectionFingerprint(belenios.computeFingerprint(inputElectionData));
    setElectionLoadingStatus(1);
  };

  const loadElectionDataFromUuid = (uuid, draft) => {
    const url = draft ? `${relativeServerRootFolder}/draft/preview/${uuid}/election.json` : `${relativeServerRootFolder}/elections/${uuid}/election.json`;
    fetch(url)
      .then(response => {
        if(!response.ok){
          setElectionLoadingErrorMessage("Error: Could not load this election. Maybe no election exists with this identifier."); // TODO: should we localize this?
          setElectionLoadingStatus(2);
        }
        else {
          response.json().then(processElectionData);
        }
      });
  };

  React.useMemo(() => {
    if(uuid){
      loadElectionDataFromUuid(uuid, draft);
    }
  }, [uuid]);


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
      loadElectionDataFromUuid(uuid, draft);
    };

    const titleMessage = t("page_title");

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
    const titleMessage = electionLoadingStatus === 0 ? "Loading..." : "Error"; // TODO: should we localize this?
    const loadingMessage = electionLoadingStatus === 0 ? titleMessage : electionLoadingErrorMessage;

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
          electionObject,
          electionFingerprint,
          currentStep
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
                alert(t("alert_invalid_credential"));
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
          electionObject,
          electionFingerprint,
          currentStep
        },
        e(
          AllQuestionsWithPagination,
          {
            electionObject,
            onVoteSubmit: async function(event, voterSelectedAnswersAsUncryptedBallot){
              setUncryptedBallotBeforeReview(voterSelectedAnswersAsUncryptedBallot);
              setCryptedBallotBeforeReview(null);
              setSmartBallotTracker(null);
              setCurrentStep(3);
              const encryptBallotSuccessCallback = (ballot, tracker) => {
                setCryptedBallotBeforeReview(ballot);
                setSmartBallotTracker(tracker);
              };
              const encryptBallotErrorCallback = (error) => {
                alert("Error: " + error);
              };
              setTimeout(function(){
                belenios.encryptBallot(
                  electionData, credential, voterSelectedAnswersAsUncryptedBallot,
                  encryptBallotSuccessCallback, encryptBallotErrorCallback
                );
              }, 50);
            }
          }
        )
      );
    }
    else if (currentStep === 3) {
      const urlToPostEncryptedBallot = `${relativeServerRootFolder}/election/submit-ballot`;
      const onClickPrevious = () => {
        setCurrentStep(2);
        setUncryptedBallotBeforeReview(null);
        setCryptedBallotBeforeReview(null);
        setSmartBallotTracker(null);
      };
      return e(
        VotePage,
        {
          electionObject,
          electionFingerprint,
          currentStep
        },
        e(
          ReviewEncryptSection,
          {
            electionObject,
            uncryptedBallot: uncryptedBallotBeforeReview,
            cryptedBallot: cryptedBallotBeforeReview,
            smartBallotTracker,
            onClickPrevious,
            urlToPostEncryptedBallot
          }
        )
      );
    }
  }
}

const VoteApp = withTranslation()(TranslatableVoteApp);

const afterI18nInitialized = (uuid, lang, credential, draft) => {
  return function(){
    document.title = i18next.t("page_title");
    document.querySelector("html").setAttribute("lang", i18next.languages[0] || "en");
    const container = document.querySelector("#vote-app");
    const root = ReactDOM.createRoot(container);
    root.render(
      e(
        VoteApp,
        {
          votingCredential: credential,
          uuid: uuid,
          draft
        }
      )
    );
  }
};

function main() {
  const hash_parameters = getHashParametersFromURL();
  const lang = hash_parameters['lang'];
  const uuid = hash_parameters['uuid'];
  const credential = hash_parameters['credential'];
  const draft = hash_parameters['draft'];
  const container = document.querySelector("#vote-app");
  container.innerHTML = "Loading...";
  i18n_init(lang, afterI18nInitialized(uuid, lang, credential, draft));
}

main();
