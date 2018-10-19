# OpenFisca Demonstrator

WARNING: this repository is deprecated and unmaintained. I now prefer to work on https://github.com/cbenz/openfisca-dash-ui

## Getting started

You need to have [Elm](http://elm-lang.org/) 0.18 installed on your machine.

Compile this project with:

    elm make src/Main.elm

Then view it:

    elm reactor --port 2025

and open http://localhost:2025/index.html

## Linting

    npm install
    npm run lint

## Production deploy

```
./deploy.sh
```

and follow the instructions printed last.
