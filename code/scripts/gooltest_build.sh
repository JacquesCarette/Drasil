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

cd "$BUILD_FOLDER$EDIR" || exit
E_DIR=$(pwd)
for lang in */; do
  cd "$lang" || exit
  LANG_DIR=$(pwd)
  for test in */; do
    cd "$test" || exit
    # shellcheck disable=SC2086
    "$MAKE" $TARGET
    RET=$(( "$RET" || $? ))
    cd "$LANG_DIR" || exit
  done
  cd "$E_DIR" || exit
done

exit $RET
