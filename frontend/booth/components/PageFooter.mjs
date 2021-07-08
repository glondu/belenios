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
      t("election_uuid_is_x",{uuid: electionUuid})
    ),
    e(
      "div",
      {
        className: "page-footer__election-footprint-container"
      },
      t("election_fingerprint_is_x",{fingerprint: electionFingerprint})
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
