function NicePasswordInput(props={}){
  return e(
    "input",
    {
      type: "password",
      className: "nice-password-input",
      ...props
    }
  );
}

export { NicePasswordInput };
export default NicePasswordInput;
