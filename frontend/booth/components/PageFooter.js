import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

function TranslatablePageFooter({ electionUuid, electionFingerprint, t }) {
  return e(
    "div",
    {
      className: "page-footer",
    },
    e(
      "div",
      {
        className: "page-footer__election-uuid-container",
      },
      t("election_uuid_is_x", { uuid: electionUuid }),
    ),
    e(
      "div",
      {
        className: "page-footer__election-footprint-container",
      },
      t("election_fingerprint_is_x", { fingerprint: electionFingerprint }),
    ),
  );
}

const PageFooter = withTranslation()(TranslatablePageFooter);

function EmptyPageFooter(props) {
  return e("div", {
    className: "page-footer",
  });
}

export { PageFooter, TranslatablePageFooter, EmptyPageFooter };
export default PageFooter;
