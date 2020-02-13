if [ -z "$EDIR" ]; then
  echo "Missing EDIR."
  exit 1
fi
if [ -z "$MAKE" ]; then
  echo "Missing MAKE."
  exit 1
fi

RET=0

if [ "$EDIR" = "Projectile" ]; then
  cd "$BUILD_FOLDER$EDIR"
  E_DIR=$(pwd)
  for d in */; do
    cd "$d"
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
    cd "$E_DIR"
  done
fi

if [ -d "$BUILD_FOLDER$EDIR/$EXAMPLE_CODE_SUBFOLDER" ]; then
  cd "$BUILD_FOLDER$EDIR/$EXAMPLE_CODE_SUBFOLDER"
  OLD_DIR=$(pwd)
  for d in */; do
    cd "$d"
    "$MAKE" $TARGET
    RET=$(( $RET || $? ))
    cd "$OLD_DIR"
  done
fi
exit $RET
