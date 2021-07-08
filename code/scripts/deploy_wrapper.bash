# Wrapper for the deploy.bash script. Used by Build.yaml.
bash $(cat "$SHELL_OPTS_FILE") scripts/deploy.bash
