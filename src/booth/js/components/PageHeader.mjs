const e = React.createElement;

function TranslatablePageHeader({title, subTitle, t}) {
  return e(
    "div",
    {
      className: "page-header"
    },
    e(
      "div",
      {
        className: "page-header__logo"
      },
      e(
        "img",
        {
          src: "./static/logo.png",
          alt: t("Election server"),
          title: t("Election server")
        }
      )
    ),
    e(
      "div",
      {
        className: "page-header__titles"
      },
      e(
        "h1",
        {
          className: "page-header__titles__election-name",
          id: "election_name",
        },
        title
      ),
      e(
        "p",
        {
          className: "page-header__titles__election-description",
          id: "election_description"
        },
        subTitle
      )
    ),
    e(
      "div",
      {
        className: "page-header__right"
      }
    )
  );
}

TranslatablePageHeader.defaultProps = {
  title: "Title of election",
  subTitle: "Subtitle of election",
  logoAlt: "Election server"
};

const PageHeader = ReactI18next.withTranslation()(TranslatablePageHeader);

export { PageHeader, TranslatablePageHeader };
export default PageHeader;
