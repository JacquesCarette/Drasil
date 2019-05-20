if [ -z "$EDIR" ]; then
  echo "Missing EDIR."
  exit 1
fi
if [ -z "$MAKE" ]; then
  echo "Missing MAKE."
  exit 1
fi

RET=0

if [ -d "$BUILD_FOLDER$EDIR/$EXAMPLE_CODE_SUBFOLDER" ]; then
  cd "$BUILD_FOLDER$EDIR/$EXAMPLE_CODE_SUBFOLDER"
  OLD_DIR=$(pwd)
  for d in */; do
    cd "$d"
    "$MAKE"
    RET=$(( $RET || $? ))
    cd "$OLD_DIR"
  done
fi
exit $RET
