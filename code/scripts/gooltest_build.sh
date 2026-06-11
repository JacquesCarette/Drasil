#!/usr/bin/env bash

if [ -z "$CODE_DIR" ]; then
  echo "Missing EDIR."
  exit 1
fi
if [ -z "$MAKE" ]; then
  echo "Missing MAKE."
  exit 1
fi

RET=0

cd "$CODE_DIR" || exit 1
E_DIR=$(pwd)
for test in */; do
  cd "$test" || exit 1
  TEST_DIR=$(pwd)
  for lang in */; do
    cd "$lang" || exit 1
    # shellcheck disable=SC2086
    "$MAKE" $TARGET
    RET=$(( RET || $? ))
    cd "$TEST_DIR" || exit 1
  done
  cd "$E_DIR" || exit 1
done

exit $RET
