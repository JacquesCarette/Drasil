if [ -z "$LABEL_FILE" ]; then
	echo "Missing temp file location."
	exit 1
fi

if [ -z "$TRAVIS_PULL_REQUEST_BRANCH" ]; then
	echo "Not a pull request."
	echo "Not running post-build step."
	exit 0
fi

LABELS=$(head -c -1 $LABEL_FILE | jq -R -s -c 'split("\n")')

curl -H "Authorization: token $BOT_TOKEN" -X PUT -d "{'labels': $LABELS}" "https://api.github.com/repos/$TRAVIS_REPO_SLUG/issues/$TRAVIS_PULL_REQUEST/labels"
