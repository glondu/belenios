var CleanCSS = require('clean-css');
var input = '@import("' + process.argv[2] + '");';
var options = { /* options */ };
var output = new CleanCSS(options).minify(input);

if (output.errors.length) {
    process.stderr.write("There were errors:\n");
    output.errors.forEach((x) => process.stderr.write("  " + x + "\n"));
    process.stderr.write("\n");
    process.exit(2);
}

if (output.warnings.length) {
    process.stderr.write("There were warnings:\n");
    output.warnings.forEach((x) => process.stderr.write("  " + x + "\n"));
    process.stderr.write("\n");
}

process.stderr.write("Inlined stylesheets:\n");
output.inlinedStylesheets.forEach((x) => process.stderr.write("  " + x + "\n"));

process.stdout.write(output.styles);
