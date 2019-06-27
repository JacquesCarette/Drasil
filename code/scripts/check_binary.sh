if [ -z "$BIN" ]; then
  echo "Missing BIN"
  exit 1
fi

if [ -z "$DOWNLOAD_LOCATION" ]; then
  echo "Missing DOWNLOAD_LOCATION"
  exit 1
fi

command -v "$BIN" > /dev/null

RET=$?  # "command" returns 1 if binary not found
if [ $RET -eq 1 ]; then
  echo "Couldn't find \`$BIN\`."
  echo "It can be installed from $DOWNLOAD_LOCATION"
  exit 1
fi