import { WhiteNiceButton } from "./NiceButton.mjs";

const e = React.createElement;

function TranslatableNoUuidSection({ onClickLoadFromUuid=null, onClickLoadFromParameters=null, t }){
  const loadViaUuidMessage = t("Load an election on this server by giving its UUID");
  const loadViaParametersMessage = t("Load any election by giving its parameters");
  return e(
    "div",
    {
      style: {
        padding: "30px"
      }
    },
    e(
      "h2",
      null,
      loadViaUuidMessage
    ),
    e(
      "input",
      {
        type: "text",
        id: "uuid"
      }
    ),
    e("br"),
    e(
      WhiteNiceButton,
      {
        label: t("Load from UUID"),
        onClick: () => {
          const uuid = document.querySelector("#uuid").value;
          onClickLoadFromUuid(uuid);
        }
      }
    ),
    e(
      "h2",
      null,
      loadViaParametersMessage
    ),
    e(
      "textarea",
      {
        id: "election_params"
      }
    ),
    e("br"),
    e(
      WhiteNiceButton,
      {
        label: t("Load from parameters"),
        onClick: () => {
          const election_params = document.querySelector("#election_params").value;
          onClickLoadFromParameters(election_params);
        }
      }
    ),
  );
}

const NoUuidSection = ReactI18next.withTranslation()(TranslatableNoUuidSection);

export { NoUuidSection, TranslatableNoUuidSection };
export default NoUuidSection;
