'use strict';

import { createElement as e } from "react";

const markupFunctions = {
    text: ((key, x) => e("span", {key}, x)),
    br: ((key) => e("br", {key})),
    bold: ((key, xs) => e("span", {key, className: "markup-b"}, ...xs)),
    italic: ((key, xs) => e("span", {key, className: "markup-i"}, ...xs)),
    error: ((x) => e("span", {className: "markup-error"}, x)),
    result: ((xs) => e("span", {}, ...xs))
}

function markup(x) {
    return belenios.markup(markupFunctions, x);
}

export { markup };
