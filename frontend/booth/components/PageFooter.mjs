const e = React.createElement;

function TranslatablePageFooter({electionUuid, electionFingerprint, t}) {
  return e(
    "div",
    {
      className: "page-footer"
    },
    e(
      "div",
      {
        className: "page-footer__election-uuid-container"
      },
      t("electionUuid",{uuid: electionUuid})
    ),
    e(
      "div",
      {
        className: "page-footer__election-footprint-container"
      },
      t("electionFingerprint",{fingerprint: electionFingerprint})
    )
  );
}

TranslatablePageFooter.defaultProps = {
  electionUuid: "aaa",
  electionFingerprint: "aaaaa"
};

const PageFooter = ReactI18next.withTranslation()(TranslatablePageFooter);

function EmptyPageFooter(props) {
  return e(
    "div",
    {
      className: "page-footer"
    }
  );
}

export { PageFooter, TranslatablePageFooter, EmptyPageFooter };
export default PageFooter;
