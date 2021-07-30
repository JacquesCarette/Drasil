#!/usr/bin/env bash

# Grab list of all generated source code folders for an example.
# List items are of the form: "EXAMPLE:code/stable/EXAMPLE/src/".
# For use with `make deploy_code_path` & `make X_deploy_code_path`.

if [ -z "$EDIR" ]; then
  echo "Missing EDIR."
  exit 1
fi

if [ -z "$BUILD_FOLDER" ]; then
  echo "Missing BUILD_FOLDER."
  exit 1
fi

if [ -z "$EXAMPLE_CODE_SUBFOLDER" ]; then
  echo "Missing EXAMPLE_CODE_SUBFOLDER."
  exit 1
fi

if [ -z "$MULTI_SRC_DIRS" ]; then
  echo "Missing MULTI_SRC_DIRS."
  exit 1
fi

if [ -z "$DEPLOY_CODE_PATH_KV_SEP" ]; then
  echo "Missing DEPLOY_CODE_PATH_KV_SEP."
  exit 1
fi

if [ -z "$EXAMPLE" ]; then
  echo "Missing EXAMPLE."
  exit 1
fi

# Print as formatted K/V for `make deploy` to extract
if [ -d "$BUILD_FOLDER$EDIR/$EXAMPLE_CODE_SUBFOLDER" ]; then
  echo "$EDIR$DEPLOY_CODE_PATH_KV_SEP$(git rev-parse --show-prefix)stable/$EXAMPLE/$EXAMPLE_CODE_SUBFOLDER"
fi
if [[ "$MULTI_SRC_DIRS" == *"$EDIR"* ]]; then
  for v in "$BUILD_FOLDER$EDIR/$EDIR"*; do
    v_name=$(basename "$v")
    echo "$EDIR$DEPLOY_CODE_PATH_KV_SEP$(git rev-parse --show-prefix)stable/$EXAMPLE/$v_name/$EXAMPLE_CODE_SUBFOLDER"
  done
fi
