if [ -z "$EDIR" ]; then
  echo "Missing EDIR."
  exit 1
fi
if [ -z "$MAKE" ]; then
  echo "Missing MAKE."
  exit 1
fi

RET=0

cd "$BUILD_FOLDER$EDIR"
E_DIR=$(pwd)
for lang in */; do
  cd "$lang"
  LANG_DIR=$(pwd)
  for test in */; do
    cd "$test"
    "$MAKE" $TARGET
    RET=$(( $RET || $? ))
    cd "$LANG_DIR"
  done
  cd "$E_DIR"
done

exit $RET
