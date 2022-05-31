'use strict';

const e = React.createElement;

function txt_br(x) {
    if (x) {
        const lines = x.split(/<br>/);
        var result = [];
        if (lines.length > 0) {
            result.push(lines[0]);
            for (var i = 1; i < lines.length; i++) {
                result.push(e("br"));
                result.push(lines[i]);
            }
        }
        return result;
    } else {
        return x;
    }
}
