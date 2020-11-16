const e = React.createElement;

function TranslatablePageFooter({electionUuid, electionFootprint, t}) {
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
      t("electionFootprint",{footprint: electionFootprint})
    )
  );
}

TranslatablePageFooter.defaultProps = {
  electionUuid: "aaa",
  electionFootprint: "aaaaa"
};

const PageFooter = ReactI18next.withTranslation()(TranslatablePageFooter);

export { PageFooter, TranslatablePageFooter };
export default PageFooter;
