import { WhiteNiceButton } from "./NiceButton.mjs";

const e = React.createElement;

function TranslatableNoUuidSection({ onClickLoadFromUuid=null, onClickLoadFromParameters=null, t }){
  const loadViaUuidMessage = t("ask_election_uuid_to_load");
  const loadViaParametersMessage = t("ask_election_parameters_to_load");
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
        label: t("load_election_from_uuid_label"),
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
        label: t("load_election_from_parameters_label"),
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
