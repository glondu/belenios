import ClassicVoteReview from "./ClassicVoteReview.mjs";
import { WhiteNiceButton, BlueNiceButton, NiceButton } from "./NiceButton.mjs";

function TranslatableReviewEncryptSection({
  electionData=null, uncryptedBallot=[],
  cryptedBallot=null, smartBallotTracker=null,
  onClickGiveUp=null, onClickNext=null, t
}){
  const smartBallotTrackerId = "smart_ballot_tracker";
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
            lineHeight: "35px",
            fontFamily: "monospace"
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
    e(
      BlueNiceButton,
      {
        label: t("Next"),
        style: {
          marginLeft: "20px"
        },
        onClick: onClickNext
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
  return e(
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
  );
}

const ReviewEncryptSection = ReactI18next.withTranslation()(TranslatableReviewEncryptSection);

export { ReviewEncryptSection, TranslatableReviewEncryptSection };
export default ReviewEncryptSection;
