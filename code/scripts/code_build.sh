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
      "$MAKE" $TARGET
      RET=$(( $RET || $? ))
      cd "$OLD_DIR"
    done
  fi
}

RET=0

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
