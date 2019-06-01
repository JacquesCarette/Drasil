SOURCE_BRANCH="con_dep"
DEPLOY_BRANCH="gh-pages"
SOURCE_BRANCH="con_dep"
DEPLOY_FOLDER="deploy"
BUILD_NUMBER_FILE=".build-num"

if [ "$TRAVIS_EVENT_TYPE" != "push" ]; then
  echo "Deployment only occurs for push builds."
  exit 0
fi

if [ "$TRAVIS_BRANCH" != "$SOURCE_BRANCH" ]; then
  echo "Only perform deploys for $SOURCE_BRANCH."
  echo "Skipping."
  exit 0
fi

# This check is seemingly redundant, but we w2ant to be certain we're not deploying for the gh-pages branch as
# that could cause an infinite recursion.
if [ "$TRAVIS_BRANCH" = "$DEPLOY_BRANCH" ]; then
  echo "Tried to deploy for the deploy target."
  echo "This should never happen. Something is really off."
  echo "Failing build."
  exit 1
fi

if [ -z "$BOT_TOKEN" ]; then
  echo "Secure variables unavailable."
  echo "Failing build for this."
  exit 1
fi

if [ -z "$BOT_EMAIL" ]; then
  echo "Assuming dummy email"
  BOT_EMAIL="drasil-bot@local"
fi

source "$ALL_FUNCTIONS_FILE"

copy_docs() {
  rm -r docs
  DOC_DIR=$(stack path | grep local-doc-root | cut -d":" -f2 | sed -e "s/^ //")/
  mkdir -p docs
  cp -r "$DOC_DIR"/. docs/
}

copy_graphs() {
  rm -r graphs
  cp -r ../graphs/. graphs/
}

try_deploy() {
  git clone --quiet --branch="$DEPLOY_BRANCH" --depth=5 "https://github.com/$TRAVIS_REPO_SLUG.git" "$DEPLOY_FOLDER"
  if [ $? = 1 ]; then
    echo "Clone failed. Bailing."
    exit 1
  fi
  CUR_DIR="$PWD"
  cd "$DEPLOY_FOLDER"
  CLONED_BUILD_NUM=$(cat "$BUILD_NUMBER_FILE")

  if [ $CLONED_BUILD_NUM -ge $TRAVIS_BUILD_NUMBER ]; then
    echo "Deploy is newer than current build. Stopping here."
    return 0
  fi

  echo $TRAVIS_BUILD_NUMBER > "$BUILD_NUMBER_FILE"
  copy_docs
  copy_graphs
  echo "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>Drasil</title></head><body>Missing real index.</body></html>" > index.html

  git config user.email "$BOT_EMAIL"
  git config user.name "drasil-bot"
  git add -A .
  # We overwrite history because the artifacts we keep in here are moderately large and woiuld pollute history otherwise.
  git commit -q --allow-empty -m "drasil-bot deploy of $SOURCE_BRANCH@$TRAVIS_COMMIT"
  git push --quiet "https://$BOT_TOKEN@github.com/$TRAVIS_REPO_SLUG.git" "$DEPLOY_BRANCH"
  PUSH_RET=$?
  # Perform some cleanup so we can (optionally retry)
  cd "$CUR_DIR"
  rm -rf "$DEPLOY_FOLDER"
  # git push returns >0 if push fails (i.e. we would need to force push)
  return $PUSH_RET
}

travis_retry try_deploy
