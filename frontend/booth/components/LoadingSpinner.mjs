
function LoadingSpinner(props) {
  return e(
    "div",
    {
      className: "lds-spinner",
      ...props
    },
    e("div"),
    e("div"),
    e("div"),
    e("div"),
    e("div"),
    e("div"),
    e("div"),
    e("div"),
    e("div"),
    e("div"),
    e("div"),
    e("div"),
  );
}

export { LoadingSpinner };
export default LoadingSpinner;
