#!/usr/bin/env bash

# For examples that generate code, walk through their generated source code folders,
# and build them all.
# For use with `make X_code` & `make code`.

if [ -z "$EDIR" ]; then
  echo "Missing EDIR."
  exit 1
fi
if [ -z "$MAKE" ]; then
  echo "Missing MAKE."
  exit 1
fi

build_code () {
  if [ -d "$EXAMPLE_CODE_SUBFOLDER" ]; then
    cd "$EXAMPLE_CODE_SUBFOLDER"
    OLD_DIR=$(pwd)
    for dr in */; do
      cd "$dr"
      if [ -d "$ROOT_DIR/$DF_DIR$EDIR/$dr" ]; then
        cp -r "$ROOT_DIR/$DF_DIR$EDIR/$dr"* "."
      fi
      "$MAKE" $TARGET
      RET=$(( $RET || $? ))
      cd "$OLD_DIR"
    done
  fi
}

RET=0
ROOT_DIR=$(pwd)
cd "$BUILD_FOLDER$EDIR"
if [[ "$MULTI_SRC_DIRS" == *"$EDIR"* ]]; then
  E_DIR=$(pwd)
  for d in */; do
    cd "$d"
    build_code
    cd "$E_DIR"
  done
fi

build_code
exit $RET
