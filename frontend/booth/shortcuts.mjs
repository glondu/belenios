'use strict';

import { createElement as e } from "react";

function txt_br(x) {
    if (x) {
        const lines = x.split(/<br>/);
        var result = [];
        if (lines.length > 0) {
            result.push(e("span", {key: 0}, lines[0]));
            for (var i = 1; i < lines.length; i++) {
                result.push(e("br", {key: 2 * i - 1}));
                result.push(e("span", {key: 2 * i}, lines[i]));
            }
        }
        return result;
    } else {
        return x;
    }
}

export { txt_br };
