import React, { createElement as e } from "react";

function NicePasswordInput(props = {}) {
  return e("input", {
    type: "password",
    className: "nice-password-input",
    ...props,
  });
}

function NiceTextInput(props = {}) {
  return e("input", {
    type: "text",
    className: "nice-text-input",
    ...props,
  });
}

export { NicePasswordInput, NiceTextInput };
export default NicePasswordInput;
