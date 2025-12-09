import ReactDOM from "react-dom/client";
import React, { createElement as e } from "react";
import i18next from "i18next";
import { withTranslation } from "react-i18next";

import i18n_init from "./i18n_init.js";
import PageHeader from "./components/PageHeader.js";
import { VoteProgress } from "./components/Progress.js";
import { AllQuestionsWithPagination } from "./components/AllQuestionsWithPagination.js";
import NoUuidSection from "./components/NoUuidSection.js";
import InputCredentialSection from "./components/InputCredentialSection.js";
import ReviewEncryptSection from "./components/ReviewEncryptSection.js";
import { PageFooter, EmptyPageFooter } from "./components/PageFooter.js";
import { Election } from "./election_utils.js";

const relativeServerRootFolder = "";

function getHashParametersFromURL() {
  const url_hash_parameters = window.location.hash.substr(1);
  const hash_parameters = url_hash_parameters.split("&").reduce(function (
    result,
    item,
  ) {
    const parts = item.split("=");
    result[parts[0]] = parts[1];
    return result;
  }, {});
  let saved_hash = "";
  for (let key in hash_parameters) {
    if (key !== "credential") {
      if (saved_hash) saved_hash += "&";
      saved_hash += `${key}=${hash_parameters[key]}`;
    }
  }
  window.location.replace(`#${saved_hash}`);
  return hash_parameters;
}

function VotePage({
  electionObject,
  electionFingerprint,
  currentStep,
  children,
}) {
  React.useEffect(() => {
    belenios.setLogo(electionObject.uuid);
  }, [electionObject]);
  return e(
    "div",
    {
      className: "page",
    },
    e(PageHeader, {
      title: electionObject.title,
      subTitle: electionObject.description,
    }),
    e(
      "div",
      {
        className: "page-body",
        id: "main", // used to ease targetting of DOM elements in automated tests
      },
      e(VoteProgress, {
        currentStep: currentStep,
      }),
      children,
    ),
    e(PageFooter, {
      electionUuid: electionObject.uuid,
      electionFingerprint: electionFingerprint,
    }),
  );
}

function GenericPage({ title = null, subTitle = null, children }) {
  return e(
    "div",
    {
      className: "page",
    },
    e(PageHeader, {
      title: title,
      subTitle: subTitle,
    }),
    e(
      "div",
      {
        className: "page-body",
      },
      children,
    ),
    e(EmptyPageFooter),
  );
}

function TranslatableVoteApp({
  uuid = null,
  votingCredential = null,
  draft,
  lang,
  t,
}) {
  const [currentStep, setCurrentStep] = React.useState(
    votingCredential ? 2 : 1,
  ); // Current step of the workflow displayed in the Breadcrumb. 1: input credential. 2: answer questions. 3: review and encrypt.
  const [electionData, setElectionData] = React.useState({});
  const [electionObject, setElectionObject] = React.useState(undefined);
  const [electionFingerprint, setElectionFingerprint] = React.useState("");
  const [credential, setCredential] = React.useState(votingCredential);
  const electionModule = React.useRef(null);
  const [firstLoad, setFirstLoad] = React.useState(true);
  const [electionLoadingStatus, setElectionLoadingStatus] = React.useState(0); // 0: not yet loaded. 1: loaded with success. 2: loaded with error.
  const [electionLoadingErrorMessage, setElectionLoadingErrorMessage] =
    React.useState(null);
  const [uncryptedBallotBeforeReview, setUncryptedBallotBeforeReview] =
    React.useState(null);
  const [cryptedBallotBeforeReview, setCryptedBallotBeforeReview] =
    React.useState(null);
  const [smartBallotTracker, setSmartBallotTracker] = React.useState(null);
  const [confirmation, setConfirmation] = React.useState(false);

  React.useEffect(() => {
    if (confirmation) {
      const container = document.getElementById("authentication-done");
      if (container) {
        belenios.renderConfirmation(container);
      }
    }
  }, [confirmation]);

  const processElectionData = (inputElectionData) => {
    const onfailure = (error) => {
      setElectionLoadingErrorMessage(error);
      setElectionLoadingStatus(2);
    };
    const onsuccess = () => {
      setElectionData(inputElectionData);
      try {
        let election = new Election(inputElectionData);
        setElectionObject(election);
      } catch (error) {
        onfailure(error);
        return;
      }
      setElectionFingerprint(belenios.computeFingerprint(inputElectionData));
      setElectionLoadingStatus(1);
    };
    belenios.init({
      root: relativeServerRootFolder,
      stateful: true,
      lang,
      callbacks: { onsuccess, onfailure },
    });
  };

  const loadElectionDataFromUuid = (uuid, draft) => {
    const url = `${relativeServerRootFolder}api/elections/${uuid}/election`;
    fetch(url).then((response) => {
      if (!response.ok) {
        setElectionLoadingErrorMessage(
          "Error: Could not load this election. Maybe no election exists with this identifier.",
        ); // TODO: should we localize this?
        setElectionLoadingStatus(2);
      } else {
        response.json().then(processElectionData);
      }
    });
  };

  React.useMemo(() => {
    if (uuid) {
      loadElectionDataFromUuid(uuid, draft);
    }
  }, [uuid]);

  const changeCredential = (credential) => {
    const callbacks = {
      success: (election) => {
        electionModule.current = election;
        setCredential(credential);
        setCurrentStep(2);
      },
      failure: (error, detail) => {
        let msg = null;
        switch (error) {
          case "INVALID_CREDENTIAL":
          case "MAYBE_PASSWORD":
            msg = t("alert_invalid_credential");
            break;
          case "GENERIC_FAILURE":
            msg = "Error: " + detail;
            break;
          default:
            msg = "Unexpected error: " + error + " (" + detail + ")";
            break;
        }
        alert(msg);
        setCredential(null);
        setCurrentStep(1);
      },
    };
    setFirstLoad(false);
    belenios.checkCredential(electionData, credential, callbacks);
  };

  if (!uuid && electionLoadingStatus == 0) {
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
        subTitle: null,
      },
      e(NoUuidSection, {
        onClickLoadFromUuid: onClickLoadFromUuid,
        onClickLoadFromParameters: onClickLoadFromParameters,
      }),
    );
  } else if (electionLoadingStatus === 0 || electionLoadingStatus === 2) {
    const titleMessage = electionLoadingStatus === 0 ? "Loading..." : "Error"; // TODO: should we localize this?
    const loadingMessage =
      electionLoadingStatus === 0 ? titleMessage : electionLoadingErrorMessage;

    return e(
      GenericPage,
      {
        title: titleMessage,
        subTitle: null,
      },
      e(
        "div",
        {
          style: {
            textAlign: "center",
            padding: "30px 0",
          },
        },
        loadingMessage,
      ),
    );
  } else {
    if (currentStep === 1) {
      return e(
        VotePage,
        {
          electionObject,
          electionFingerprint,
          currentStep,
        },
        e(InputCredentialSection, {
          onSubmit: function (credential) {
            changeCredential(credential);
            return false;
          },
        }),
      );
    } else if (currentStep === 2) {
      if (firstLoad && credential) changeCredential(credential);
      return e(
        VotePage,
        {
          electionObject,
          electionFingerprint,
          currentStep,
        },
        e(AllQuestionsWithPagination, {
          electionObject,
          onVoteSubmit: async function (
            event,
            voterSelectedAnswersAsUncryptedBallot,
          ) {
            setUncryptedBallotBeforeReview(
              voterSelectedAnswersAsUncryptedBallot,
            );
            setCryptedBallotBeforeReview(null);
            setSmartBallotTracker(null);
            setCurrentStep(3);
            const callbacks = {
              success: (ballot, tracker) => {
                setCryptedBallotBeforeReview(ballot);
                setSmartBallotTracker(tracker);
              },
              failure: (error) => {
                alert("Error: " + error);
              },
            };
            setTimeout(function () {
              belenios.encryptBallot(
                electionModule.current,
                voterSelectedAnswersAsUncryptedBallot,
                callbacks,
              );
            }, 50);
          },
        }),
      );
    } else if (currentStep === 3) {
      const onClickPrevious = () => {
        setCurrentStep(2);
        setUncryptedBallotBeforeReview(null);
        setCryptedBallotBeforeReview(null);
        setSmartBallotTracker(null);
      };
      const onClickNext = () => {
        setCurrentStep(4);
        belenios.submitBallot({
          finished: () => {
            setCurrentStep(5);
            setConfirmation(true);
          },
        });
      };
      return e(
        VotePage,
        {
          electionObject,
          electionFingerprint,
          currentStep,
        },
        e(ReviewEncryptSection, {
          electionObject,
          electionModule,
          uncryptedBallot: uncryptedBallotBeforeReview,
          cryptedBallot: cryptedBallotBeforeReview,
          smartBallotTracker,
          onClickPrevious,
          onClickNext,
          draft,
        }),
      );
    } else if (currentStep === 4) {
      return e(VotePage, {
        electionObject,
        electionFingerprint,
        currentStep,
      });
    } else if (currentStep === 5) {
      return e(
        VotePage,
        {
          electionObject,
          electionFingerprint,
          currentStep,
        },
        e("div", { id: "authentication-done" }),
      );
    }
  }
}

const VoteApp = withTranslation()(TranslatableVoteApp);

const afterI18nInitialized = (uuid, lang, credential, draft) => {
  return function () {
    document.title = i18next.t("page_title");
    document
      .querySelector("html")
      .setAttribute("lang", i18next.languages[0] || "en");
    const container = document.querySelector("#vote-app");
    const root = ReactDOM.createRoot(container);
    root.render(
      e(VoteApp, {
        votingCredential: credential,
        uuid: uuid,
        draft,
        lang,
      }),
    );
  };
};

function main() {
  const hash_parameters = getHashParametersFromURL();
  const lang = hash_parameters["lang"];
  const uuid = hash_parameters["uuid"];
  const draft = hash_parameters["draft"];
  const credential = draft
    ? "XXXXX-XXXXXX-XXXXX-XXXXXX"
    : hash_parameters["credential"];
  const container = document.querySelector("#vote-app");
  container.innerHTML = "Loading...";
  i18n_init(lang, afterI18nInitialized(uuid, lang, credential, draft));
}

main();
