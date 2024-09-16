import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

import WholeVoteRecap from "./WholeVoteRecap.js";
import { WhiteNiceButton, BlueNiceButton, NiceButton } from "./NiceButton.js";
import LoadingSpinner from "./LoadingSpinner.js";

function TranslatableReviewEncryptSection({
  electionObject = null,
  electionModule = null,
  uncryptedBallot = [],
  cryptedBallot = null,
  smartBallotTracker = null,
  onClickPrevious = null,
  onClickNext = null,
  draft = null,
  t,
}) {
  // identifiers are copied from original booth
  const smartBallotTrackerId = "smart_ballot_tracker";
  const ballotContainerId = "ballot_div";
  const ballotFormId = "ballot_form";
  const encryptedBallotId = "ballot";
  const encryptedBallotName = "encrypted_vote";

  let tracker = {};
  if (smartBallotTracker) {
    tracker = belenios.formatTracker(
      electionModule.current,
      smartBallotTracker,
    );
  }
  const contentWhenBallotIsBeingEncrypted = e(
    "div",
    null,
    e("div", null, t("ask_to_wait_during_ballot_encryption")),
    e(LoadingSpinner, {
      style: {
        marginTop: "15px",
      },
    }),
  );
  function setBrowserSelectionToSmartBallotTracker() {
    let el = document.getElementById(smartBallotTrackerId);
    const range = document.createRange();
    range.selectNodeContents(el);
    const selection = window.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
  }
  const contentWhenBallotHasBeenEncrypted = e(
    "div",
    null,
    e("p", null, t("your_ballot_has_been_encrypted")),
    e(
      "div",
      null,
      e("span", null, t("your_smart_ballot_tracker_is")),
      e(
        "div",
        {
          className: "review-encrypt-section__smart-ballot-tracker-container",
        },
        e(
          "span",
          {
            className: "review-encrypt-section__smart-ballot-tracker",
            id: smartBallotTrackerId,
            onClick: setBrowserSelectionToSmartBallotTracker,
          },
          smartBallotTracker,
        ),
      ),
      e(
        "div",
        null,
        e("span", null, t("ask_to_save_your_smart_ballot_tracker")),
        e(WhiteNiceButton, {
          tagName: "a",
          label: t("download_tracker_label"),
          href: tracker.contents,
          download: tracker.filename,
          style: {
            marginLeft: "5px",
          },
        }),
      ),
    ),
  );
  const content = cryptedBallot
    ? contentWhenBallotHasBeenEncrypted
    : contentWhenBallotIsBeingEncrypted;

  const navigationButtonStyle = {
    padding: "10px 13px",
    minWidth: "38px",
  };
  const previousButton = e(NiceButton, {
    tagName: "a",
    label: t("previous_button_label"),
    style: {
      ...navigationButtonStyle,
      marginRight: "20px",
    },
    onClick: onClickPrevious,
  });
  const nextOnClick =
    draft == 2
      ? () => {
          window.close();
          return false;
        }
      : onClickNext;
  const paginationWhenBallotHasBeenEncrypted = e(
    "div",
    {
      style: {
        marginTop: "20px",
        textAlign: "center",
      },
    },
    previousButton,
    e(BlueNiceButton, {
      onClick: nextOnClick,
      label: t("next_button_label"),
      style: {
        ...navigationButtonStyle,
        marginLeft: "20px",
      },
    }),
  );
  const paginationWhenBallotIsBeingEncrypted = e(
    "div",
    {
      style: {
        marginTop: "20px",
        textAlign: "center",
      },
    },
    previousButton,
  );
  const pagination = cryptedBallot
    ? paginationWhenBallotHasBeenEncrypted
    : paginationWhenBallotIsBeingEncrypted;
  const encryptedBallotField = e(
    // add a hidden textarea in DOM which contains the encrypted vote, in the same way than the original booth
    "div",
    {
      style: {
        display: "none",
      },
    },
    t("encrypted_ballot_is"),
    e("textarea", {
      id: encryptedBallotId,
      name: encryptedBallotName,
      readOnly: "readonly",
      cols: "80",
      rows: "1",
      value: cryptedBallot ? cryptedBallot : undefined,
    }),
  );
  return e(
    "div",
    {
      id: ballotContainerId,
    },
    e(
      "div",
      {
        id: ballotFormId,
      },
      encryptedBallotField,
      e(
        "div",
        {
          className: "review-encrypt-section",
        },
        e("h2", null, t("review_ballot_for_questions")),
        e(WholeVoteRecap, {
          electionObject,
          uncryptedBallot,
        }),
        e(
          "div",
          {
            className: "review-encrypt-section__encryption-section",
          },
          content,
        ),
        pagination,
      ),
    ),
  );
}

const ReviewEncryptSection = withTranslation()(
  TranslatableReviewEncryptSection,
);

export { ReviewEncryptSection, TranslatableReviewEncryptSection };
export default ReviewEncryptSection;
