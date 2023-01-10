import React, { createElement as e } from "react";

function DisplayDependingOnWindowWidth({ widthLimit, smallComponent, bigComponent, ...props }) {
  const evaluateBelowLimit = () => {
    return window.innerWidth < widthLimit;
  };
  const [belowLimit, setBelowLimit] = React.useState(evaluateBelowLimit());

  const updateMedia = () => {
    setBelowLimit(evaluateBelowLimit());
  };

  React.useEffect(() => {
    window.addEventListener("resize", updateMedia);
    return () => window.removeEventListener("resize", updateMedia);
  });

  return e(
    belowLimit ? smallComponent : bigComponent,
    props
  );
}

export { DisplayDependingOnWindowWidth };
export default DisplayDependingOnWindowWidth;
