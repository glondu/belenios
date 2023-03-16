import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";
import { markup } from "../shortcuts.mjs";

function TranslatablePageHeader({title, subTitle, t}) {
  return e(
    "div",
    {
      id: "header", // used to ease targetting of DOM elements in automated tests
      className: "page-header"
    },
    e(
      "div",
      {
        "aria-hidden": true,
        className: "page-header__logo"
      },
      e(
        "img",
        {
          className: "page-header__logo__image",
          src: "../../../LOGO",
          alt: t("election_server"),
          title: t("election_server")
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
        markup(title)
      ),
      e(
        "p",
        {
          className: "page-header__titles__election-description",
          id: "election_description"
        },
        markup(subTitle)
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

const PageHeader = withTranslation()(TranslatablePageHeader);

export { PageHeader, TranslatablePageHeader };
export default PageHeader;
