# For recreating the deployment on a local machine.
# Only use from `code/` folder after running through all CI steps, `sh code/mock_deploy.sh`.

LABEL_FILE=/tmp/ci_build_failures \
MANAGED_LABEL_FILE=/tmp/ci_managed_labels \
ALL_FUNCTIONS_FILE=$(mktemp) \
SHELL_OPTS_FILE=$(mktemp) \
GITHUB_REF=refs/heads/master \
BOT_TOKEN=test \
GITHUB_RUN_ID=10001 \
GITHUB_RUN_NUMBER=10012 \
GITHUB_SHA="5fbcb79c17aa8c7820abac644ddb23a38ea1ee64" \
GITHUB_REPOSITORY=JacquesCarette/Drasil \
bash scripts/deploy_wrapper.bash
