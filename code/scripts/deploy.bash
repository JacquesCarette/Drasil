SOURCE_BRANCH="master"
DEPLOY_BRANCH="gh-pages"
DEPLOY_FOLDER="deploy/"
BUILD_NUMBER_FILE=".build-num"
COMMIT_HASH_FILE=".commit-hash"

if [ "$GITHUB_REF" != "$SOURCE_BRANCH" ]; then
  echo "Only perform deploys for $SOURCE_BRANCH."
  echo "Skipping."
  exit 0
fi

# This check is seemingly redundant, but we want to be certain we're not deploying for the gh-pages branch as
# that could cause an infinite recursion.
if [ "$GITHUB_REF" = "$DEPLOY_BRANCH" ]; then
  echo "Tried to deploy for the deploy target."
  echo "This should never happen. Something is really off."
  echo "Failing build."
  exit 1
fi

# if [ -z "$BOT_TOKEN" ]; then
#   echo "Secure variables unavailable."
#   echo "Failing build for this."
#   exit 1
# fi

if [ -z "$BOT_EMAIL" ]; then
  echo "Assuming dummy email"
  BOT_EMAIL="drasil-bot@local"
fi

try_deploy() {
  # Cleanup the deploy folder if it already exists.
  rm -rf "$DEPLOY_FOLDER" >/dev/null 2>&1
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
  cd "$DEPLOY_FOLDER"

  git config user.email "$BOT_EMAIL"
  git config user.name "drasil-bot"
  # git add -A .
  # We overwrite history because the artifacts we keep in here are moderately large and would pollute history otherwise.
  # git commit -q --allow-empty -m "drasil-bot deploy of $SOURCE_BRANCH@$GITHUB_SHA"
  # git push --quiet "https://$BOT_TOKEN@github.com/$GITHUB_REPOSITORY.git" "$DEPLOY_BRANCH"
  PUSH_RET=$?
  # Perform some cleanup so we can (optionally retry)
  cd "$CUR_DIR"
  # rm -rf "$DEPLOY_FOLDER"
  # git push returns >0 if push fails (i.e. we would need to force push)
  return $PUSH_RET
}

try_deploy
