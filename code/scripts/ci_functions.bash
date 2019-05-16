if [ -z "$LABEL_FILE" ]; then
  echo "Missing temp file location."
  exit 1
fi

if [ -z "$MANAGED_LABEL_FILE" ]; then
  echo "Missing managed temp file location."
  exit 1
fi

if [ -z "$ALL_FUNCTIONS_FILE" ]; then
  echo "Missing temp file name to export all loaded bash functions."
  exit 1
fi

if [ -z "$TRAVIS_CMD" ]; then
  echo "Expected script to be running under CI. Stopping."
  exit 1
fi

ci_export_funcs() {
  declare -f > "$ALL_FUNCTIONS_FILE"
}

ci_fstep() {
  if [ -z "$1" ]; then
    echo "Missing label"
    return 1
  fi
  local label="$1"

  shift 1

  $SHELL $(shopt | grep -E "on$" | cut -f1 | sed -E "s/^([^ ]*) *$/-O \1/g") -c "source $ALL_FUNCTIONS_FILE; $*"
  local ret=$?

  echo "$label" >> "$MANAGED_LABEL_FILE"
  if [ $ret -ne "0" ]; then
    echo "$label" >> "$LABEL_FILE"
  fi

  return $ret
}