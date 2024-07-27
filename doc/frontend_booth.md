# Documentation about the frontend folder and its autonomous booth page



The `frontend` folder contains a booth page, with its Javascript application and several User Interface components meant to be used in [Belenios](https://www.belenios.org/). They are implemented using Javascript ES6 and React (without JSX nor transpilation), and use i18next for internationalization.

## Using it: Executing the booth

### Download required packages

Go into the `frontend` directory: `cd frontend`

Install npm, optionnaly using nvm (if you choose to use nvm, don't forget to execute `nvm use current` before executing the following commands)

Then install the minimum dependencies required for this project (React, i18next, etc): `npm install`

### Execute an HTTP server to display the vote.html page

The `frontend/booth/vote.html` page is autonomous as it can be accessed without any instance of the Belenios server running: it simply need to refer to an election.json file located in a folder named after the uuid of the election. But this page uses Javascript Modules, and as stated in the [Mozilla Developer Network page about Javascript Modules](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules): "You need to pay attention to local testing — if you try to load the HTML file locally (i.e. with a `file://` URL), you'll run into CORS errors due to JavaScript module security requirements. You need to do your testing through a server.".

So you need to execute an HTTP server to display this `vote.html` page. You can run any HTTP server using the `frontend` folder as root, but a shortcut command using Python3's default HTTP server is provided in `package.json`, so with your terminal located in the `frontend` folder, simply execute the following command:

`npm start`

Then you can point your browser at http://localhost:8000/booth/vote.html

If you create an `elections` folder, containing a folder named after the uuid of your election, containing a valid `election.json` file, then you can test how this election displays on the booth page by pointing your browser at http://localhost:8000/booth/vote.html#uuid=__my_uuid__&lang=fr

For the moment, supported languages (in hash parameter of the URL) are "en" and "fr", with the default being "en".

## Creating a build and installing it somewhere

Execute `make` to create a build in the `frontend/build` folder.

Execute `make DESTDIR=../_run/usr/share/belenios-server` to create a build in the `../_run/usr/share/belenios-server/frontend` folder. This is what the `build-*-server` targets do in the `Makefile` at the root of the Belenios repository.

## Contributing to the development of the frontend vote application

### React controlled versus uncontrolled components for input fields

In React we chose to use [uncontrolled components](https://en.reactjs.org/docs/uncontrolled-components.html) for input fields (instead of controlled components), so that it remains easy (or feasable) to test the frontend vote application using end-to-end tests which control the browser.

### Extracting translatable strings from Javascript files

We use `i18next` and `react-i18next` libraries for internationalization. To extract translatable strings from Javascript files and merge them into the already existing `frontend/translations/*.json` translation files of each language, you can execute the following command:

`npm run i18n-extract`

### Reduce loading time by telling the browser early which Javascript files to download

We use HTML tags `<link rel="modulepreload">` to tell the browser to download all necessary Javascript files, instead of downloading in a waterfall way (downloading `app.mjs` then parsing it and then downloading the files it imports, continuing recursively). So if you add a Javascript file to the vote application (for example you create a new component), make sure to add it to the list of preloaded files in `vote.html` and in its `vote_development.html` counterpart. You can use the following command to generate the whole list:

`cd booth; ls -1 *.mjs components/*.mjs | grep -v ".stories.mjs" | sed -e 's/\(.*\)/<link rel="modulepreload" href="\1">/' > preload.txt`
