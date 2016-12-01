#!/bin/bash
if [[ "$TRAVIS_BRANCH" != "" ]] && [[ $TRAVIS_BRANCH == seisei-* ]]; then
  echo "Deploying branch $TRAVIS_BRANCH"
  lein ci-deploy
else
  echo "Not deploying $TRAVIS_BRANCH"
fi
