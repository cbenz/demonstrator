#!/bin/bash

REV=$(git rev-parse HEAD)

rm -rf build
rm -rf elm-stuff/build-artifacts # To display all the warnings, do not keep old compilation results.
npm run build

pushd build
git init
git add .
git commit -m "Build application from commit $REV"
git push --force git@github.com:openfisca/demonstrator.git master:dist
popd

git fetch

echo "On the server do:"
echo "git checkout dist; git fetch; git reset --hard origin/dist"
