import ClassicVoteReview from "./ClassicVoteReview.mjs";
import { WhiteNiceButton } from "./NiceButton.mjs";

function TranslatableReviewEncryptSection({ electionData=null, uncryptedBallot=[], cryptedBallot=null, t }){
  const contentWhenBallotIsBeingEncrypted = e(
    "div",
    null,
    t("pleaseWaitDuringBallotEncryption")
  );
  const contentWhenBallotHasBeenEncrypted = e(
    "div",
    null,
    t("ballotHasBeenEncrypted") // TODO: continue implementation, use ballot tracker, etc
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
          padding: "20px",
          textAlign: "center",
          marginTop: "40px"
        }
      },
      content
    )
  );
}

const ReviewEncryptSection = ReactI18next.withTranslation()(TranslatableReviewEncryptSection);

export { ReviewEncryptSection, TranslatableReviewEncryptSection };
export default ReviewEncryptSection;
