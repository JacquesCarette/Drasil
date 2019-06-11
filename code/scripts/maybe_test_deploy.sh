# This script may be a little overzealous in testing deploy (without actually deploying) based on what git diff returns.

if [ -z "$TRAVIS_PULL_REQUEST_BRANCH" ]; then
  exit 0
fi

CHANGED_FILES=$(git diff --name-only "$TRAVIS_BRANCH" | grep "website/" | wc -l)
if [ $CHANGED_FILES -gt 0 ]; then
  echo "Looks like there were changes to the deploy website."
  make deploy_lite
  RET=$?
  echo "Printing index (in case template splicing went awry)"
  cat "website/_site/index.html"
  RET=$(( $RET || $? ))
  exit $RET
else
  echo "No changes to test."
fi
