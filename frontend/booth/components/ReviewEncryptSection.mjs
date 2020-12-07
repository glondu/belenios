import ClassicVoteReview from "./ClassicVoteReview.mjs";
import { WhiteNiceButton } from "./NiceButton.mjs";

function TranslatableReviewEncryptSection({
  electionData=null, uncryptedBallot=[],
  cryptedBallot=null, smartBallotTracker=null, t
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
        "Numéro de suivi :"
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
    )
  );
}

const ReviewEncryptSection = ReactI18next.withTranslation()(TranslatableReviewEncryptSection);

export { ReviewEncryptSection, TranslatableReviewEncryptSection };
export default ReviewEncryptSection;
