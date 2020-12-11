const defaultDangerousLabel = {
  __html: "hi"
};

// Parameter `tagName` can be "button", "input", "a"
function NiceButton ({ label=null, dangerousLabel=null, onClick=null, disabled=false, styling="default", className=null, tagName="button", ...props }){
  const attributes = {
    className: className ? `nice-button nice-button--${styling} ${className}` : `nice-button nice-button--${styling}`,
    onClick: onClick,
    disabled: disabled
  };
  let bonusAttributes = {};
  if (!label && dangerousLabel){
    bonusAttributes['dangerouslySetInnerHTML'] = dangerousLabel;
  }
  if (tagName == "input"){
    bonusAttributes['type'] = 'submit';
    bonusAttributes['value'] = label;
  }
  return e(
    tagName,
    {
      ...attributes,
      ...bonusAttributes,
      ...props
    },
    tagName == "input" ? null : label
  );
}

function BlueNiceButton(props){
  return NiceButton(
    {
      ...props,
      styling: "blue"
    }
  );
}

function WhiteNiceButton(props){
  return NiceButton(
    {
      ...props,
      styling: "white"
    }
  );
}

export { NiceButton, BlueNiceButton, WhiteNiceButton };
export default NiceButton;
