import { WhiteNiceButton } from "./NiceButton.mjs";

const e = React.createElement;

function TranslatableInputCredentialSection({ onSubmit=null, t }){
  const onClick = () => {
    const result = prompt(t("Please enter your credential"));
    if (result && onSubmit){
      return onSubmit(result);
    }
  };
  return e(
    "div",
    {
      className: "input-credential-section-container",
      style: {
        padding: "30px"
      }
    },
    e(
      "div",
      {
        className: "input-credential-section",
        style: {
          background: "#e6f8fd",
          padding: "20px",
          textAlign: "center"
        }
      },
      e(
        "div",
        {
          className: "input-credential-section__instruction-container",
          style: {
            maxWidth: "300px",
            margin: "0 auto"
          }
        },
        e(
          "div",
          {
            className: "input-credential-section__instruction",
            style: {
              paddingBottom: "12px"
            }
          },
          t("Input your credential")
        ),
        e(
          WhiteNiceButton,
          {
            className: "input-credential-section__button",
            style: {
              width: "100%"
            },
            label: t("here"),
            onClick: onClick
          }
        )
      )
    )
  );
}

const InputCredentialSection = ReactI18next.withTranslation()(TranslatableInputCredentialSection);

export { InputCredentialSection, TranslatableInputCredentialSection };
export default InputCredentialSection;
