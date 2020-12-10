const defaultDangerousLabel = {
  __html: "hi"
};

function NiceButton ({ label=null, dangerousLabel=null, onClick=null, disabled=false, styling="default", className=null, asInput=false, ...props }){
  const attributes = {
    className: `nice-button nice-button--${styling} ${className}`,
    onClick: onClick,
    disabled: disabled
  };
  let bonusAttributes = {};
  if (!label && dangerousLabel){
    bonusAttributes['dangerouslySetInnerHTML'] = dangerousLabel;
  }
  if (asInput){
    bonusAttributes['type'] = 'submit';
    bonusAttributes['value'] = label;
  }
  return e(
    asInput ? 'input' : 'button',
    {
      ...attributes,
      ...bonusAttributes,
      ...props
    },
    asInput ? null : label
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
