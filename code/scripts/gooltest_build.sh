#!/usr/bin/env bash

if [ -z "$EDIR" ]; then
  echo "Missing EDIR."
  exit 1
fi
if [ -z "$MAKE" ]; then
  echo "Missing MAKE."
  exit 1
fi

RET=0

cd "$BUILD_FOLDER$EDIR" || exit 1
E_DIR=$(pwd)
for lang in */; do
  cd "$lang" || exit 1
  LANG_DIR=$(pwd)
  for test in */; do
    cd "$test" || exit 1
    # shellcheck disable=SC2086
    "$MAKE" $TARGET
    RET=$(( "$RET" || $? ))
    cd "$LANG_DIR" || exit 1
  done
  cd "$E_DIR" || exit 1
done

exit $RET
