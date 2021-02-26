import WholeVoteRecap from "./WholeVoteRecap.mjs";
import { WhiteNiceButton, BlueNiceButton, NiceButton } from "./NiceButton.mjs";
import LoadingSpinner from "./LoadingSpinner.mjs";

function TranslatableReviewEncryptSection({
  electionData=null, uncryptedBallot=[],
  cryptedBallot=null, smartBallotTracker=null,
  onClickPrevious=null, urlToPostEncryptedBallot="", t
}){
  const smartBallotTrackerId = "smart_ballot_tracker"; // identifier copied from original booth
  const contentWhenBallotIsBeingEncrypted = e(
    "div",
    null,
    e(
      "div",
      null,
      t("pleaseWaitDuringBallotEncryption")
    ),
    e(
      LoadingSpinner,
      {
        style: {
          marginTop: "15px"
        }
      }
    )
  );
  function setBrowserSelectionToSmartBallotTracker(){
    let el = document.getElementById(smartBallotTrackerId);
    const range = document.createRange();
    range.selectNodeContents(el);
    const selection = window.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
  }
  function copyToClipboard() {
    setBrowserSelectionToSmartBallotTracker();
    document.execCommand("copy");
    alert(t("yourSmartBallotTrackerHasBeenCopied"));
  }
  const contentWhenBallotHasBeenEncrypted = e(
    "div",
    null,
    e(
      "p",
      null,
      t("ballotHasBeenEncrypted")
    ),
    e(
      "div",
      null,
      e(
        "span",
        null,
        t("yourSmartBallotTracker")
      ),
      e(
        "div",
        {
          className: "review-encrypt-section__smart-ballot-tracker-container"
        },
        e(
          "span",
          {
            className: "review-encrypt-section__smart-ballot-tracker",
            id: smartBallotTrackerId,
            onClick: setBrowserSelectionToSmartBallotTracker
          },
          smartBallotTracker
        )
      ),
      e(
        "div",
        null,
        e(
          "span",
          null,
          t("pleaseSaveYourSmartBallotTracker")
        ),
        e(
          WhiteNiceButton,
          {
            tagName: "a",
            label: t("Copy"),
            onClick: copyToClipboard,
            style: {
              marginLeft: "5px"
            }
          }
        )
      )
    )
  );
  const content = cryptedBallot ? contentWhenBallotHasBeenEncrypted : contentWhenBallotIsBeingEncrypted;

  const navigationButtonStyle = {
    padding: "10px 13px",
    minWidth: "38px"
  };
  const previousButton = e(
    NiceButton,
    {
      tagName: "a",
      label: t("Previous"),
      style: {
        ...navigationButtonStyle,
        marginRight: "20px"
      },
      onClick: onClickPrevious
    }
  );
  const paginationWhenBallotHasBeenEncrypted = e(
    "div",
    {
      style: {
        marginTop: "20px",
        textAlign: "center"
      }
    },
    previousButton,
    e( // this Next button submits the form
      BlueNiceButton,
      {
        label: t("Next"),
        style: {
          ...navigationButtonStyle,
          marginLeft: "20px"
        }
      }
    )
  );
  const paginationWhenBallotIsBeingEncrypted = e(
    "div",
    {
      style: {
        marginTop: "20px",
        textAlign: "center"
      }
    },
    previousButton
  );
  const pagination = cryptedBallot ? paginationWhenBallotHasBeenEncrypted : paginationWhenBallotIsBeingEncrypted;
  const encryptedBallotField = e( // add a hidden textarea in DOM which contains the encrypted vote, in the same way than the original booth
    "div",
    {
      style: {
        display: "none"
      }
    },
    t("encryptedBallot"),
    e(
      "textarea",
      {
        id: "ballot", // identifier copied from original booth
        name: "encrypted_vote", // identifier copied from original booth
        readOnly: "readonly",
        cols: "80",
        rows: "1",
        value: cryptedBallot ? cryptedBallot : undefined
      }
    )
  );
  return e(
    "form",
    {
      id: "ballot_form", // identifier copied from original booth
      method: "POST",
      action: urlToPostEncryptedBallot,
      encType: "multipart/form-data"
    },
    encryptedBallotField,
    e(
      "div",
      {
        className: "review-encrypt-section"
      },
      e(
        "h2",
        null,
        t("reviewBallotForQuestions", {count: uncryptedBallot.length})
      ),
      e(
        WholeVoteRecap,
        {
          electionData: electionData,
          uncryptedBallot: uncryptedBallot
        }
      ),
      e(
        "div",
        {
          className: "review-encrypt-section__encryption-section"
        },
        content
      ),
      pagination
    )
  );
}

const ReviewEncryptSection = ReactI18next.withTranslation()(TranslatableReviewEncryptSection);

export { ReviewEncryptSection, TranslatableReviewEncryptSection };
export default ReviewEncryptSection;
