"use strict";

import { createElement as e } from "react";

const markupFunctions = {
  text: (key, x) => e("span", { key }, x),
  bold: (key, xs) => e("span", { key, className: "markup-b" }, ...xs),
  italic: (key, xs) => e("span", { key, className: "markup-i" }, ...xs),
  link: (key, target, label) =>
    e("a", { key, href: target, target: "_blank" }, label),
  error: (x) => e("span", { className: "markup-error" }, x),
  result: (xs) => e("span", {}, ...xs),
};

function markup(x) {
  return belenios.markup(markupFunctions, x);
}

export { markup };
