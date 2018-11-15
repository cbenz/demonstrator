# OpenFisca Demonstrator

WARNING: this repository is deprecated and unmaintained. I now prefer to work on https://github.com/cbenz/openfisca-dash-ui

## Getting started

This project is written in [elm](http://elm-lang.org/) 0.18, and uses
[create-elm-app](https://github.com/halfzebra/create-elm-app). To install it,
and the project dependencies:

```shell
npm install create-elm-app
npm install
```

To start the website locally:

```shell
npm run start
```

## Contributing

We like cleanly formatted code! To do that, we use tools like

- [eslint](https://www.npmjs.com/package/eslint) for javascript code (in the ports)
- [elm-format](https://www.npmjs.com/package/elm-format) for elm code

To run the automatic linting:

```shell
npm run lint
```

## Production deploy

First build the project:

```shell
npm run build
```

Then deploy using [gh-pages](https://www.npmjs.com/package/gh-pages)

```shell
npm run deploy [-- -o <your repository>]
```

To deploy on OpenFisca's own servers:

```shell
./deploy.sh
```

and follow the instructions printed last.
