#!/usr/bin/env bash

# Deploy script. Calls target deploy_lite from code/Makefile

SOURCE_BRANCH="master"
DEPLOY_BRANCH="gh-pages"
DEPLOY_FOLDER="deploy/"
BUILD_NUMBER_FILE=".build-num"
COMMIT_HASH_FILE=".commit-hash"

# Usually only deploy from master branch.
if [ "$GITHUB_REF" != "refs/heads/$SOURCE_BRANCH" ]; then
  echo "Only perform deploys for $SOURCE_BRANCH (attempted on: $GITHUB_REF)."
  echo "Skipping."
  exit 0
fi

# This check is seemingly redundant, but we want to be certain we're not deploying for the gh-pages branch as
# that could cause an infinite recursion.
if [ "$GITHUB_REF" = "refs/heads/$DEPLOY_BRANCH" ]; then
  echo "Tried to deploy for the deploy target."
  echo "This should never happen. Something is really off."
  echo "Failing build."
  exit 1
fi

try_deploy() {
  # Cleanup the deploy folder if it already exists.
  rm -rf "$DEPLOY_FOLDER"
  git clone --quiet --branch="$DEPLOY_BRANCH" --depth=5 "https://github.com/$GITHUB_REPOSITORY.git" "$DEPLOY_FOLDER"
  if [ $? = 1 ]; then
    echo "Clone failed. Bailing."
    exit 1
  fi
  CUR_DIR="$PWD"
  cd "$DEPLOY_FOLDER"
  CLONED_BUILD_NUM=$(cat "$BUILD_NUMBER_FILE")

  if [ "$CLONED_BUILD_NUM" -ge "$GITHUB_RUN_NUMBER" ]; then
    echo "Deploy is newer than current build. Stopping here."
    return 0
  fi

  CLONED_HASH=$(cat "$COMMIT_HASH_FILE")

  if [ $CLONED_HASH = $GITHUB_SHA ]; then
    echo "Deploy would be from same hash as current. Skipping it."
    return 0
  fi

  echo $GITHUB_RUN_NUMBER > "$BUILD_NUMBER_FILE"
  echo $GITHUB_SHA > "$COMMIT_HASH_FILE"
  cd "$CUR_DIR"
  make deploy_lite DEPLOY_FOLDER="$DEPLOY_FOLDER"
  MAKE_RET=$?
  if [ $MAKE_RET != 0 ]; then
    echo "Making the deploy folder failed! Failing deploy."
    return $MAKE_RET
  fi
  return $MAKE_RET
}

try_deploy
