import ClassicVoteReview from "./ClassicVoteReview.mjs";
import { WhiteNiceButton, BlueNiceButton, NiceButton } from "./NiceButton.mjs";

function TranslatableReviewEncryptSection({
  electionData=null, uncryptedBallot=[],
  cryptedBallot=null, smartBallotTracker=null,
  onClickGiveUp=null, urlToPostEncryptedBallot="", t
}){
  const smartBallotTrackerId = "smart_ballot_tracker"; // identifier copied from original booth
  const contentWhenBallotIsBeingEncrypted = e(
    "div",
    null,
    t("pleaseWaitDuringBallotEncryption")
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
        "span",
        {
          id: smartBallotTrackerId,
          style: {
            marginLeft: "5px",
            wordBreak: "break-all",
            border: "1px solid #ccc",
            borderRadius: "8px",
            padding: "5px 14px",
            background: "white",
            lineHeight: "32px",
            fontFamily: "monospace",
            verticalAlign: "middle",
            fontSize: "10px"
          },
          onClick: setBrowserSelectionToSmartBallotTracker
        },
        smartBallotTracker
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

  const restartButton = e(
    NiceButton,
    {
      tagName: "a",
      label: t("Restart"),
      style: {
        marginRight: "20px"
      },
      onClick: onClickGiveUp
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
    restartButton,
    e( // this Next button submits the form
      BlueNiceButton,
      {
        label: t("Next"),
        style: {
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
    restartButton
  );
  const pagination = cryptedBallot ? paginationWhenBallotHasBeenEncrypted : paginationWhenBallotIsBeingEncrypted;
  const encryptedBallotField = e(
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
        className: "review-encrypt-section",
        style: {
          padding: "0 30px 30px 30px"
        }
      },
      e(
        "h2",
        null,
        t("reviewBallotForQuestions", {count: uncryptedBallot.length})
      ),
      e(
        ClassicVoteReview,
        {
          electionData: electionData,
          uncryptedBallot: uncryptedBallot
        }
      ),
      e(
        "div",
        {
          className: "review-encrypt-section__encryption-section",
          style: {
            background: "#e6f8fd",
            borderRadius: "8px",
            padding: "20px",
            textAlign: "center",
            margin: "40px auto 0 auto",
            maxWidth: "600px"
          }
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
