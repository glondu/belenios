import React, { createElement as e } from "react";

// Parameter `tagName` can be "button", "input", "a"
function NiceButton ({ label=null, styling="default", className=null, tagName="button", ...props }){
  props["className"] = className ? `nice-button nice-button--${styling} ${className}` : `nice-button nice-button--${styling}`;
  if (tagName == "input"){
    props['type'] = 'submit';
    props['value'] = label;
  }
  return e(
    tagName,
    props,
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
