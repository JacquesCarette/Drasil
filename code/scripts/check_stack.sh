#!/usr/bin/env bash

# Ensure Stack version is at least our designated minimum, caching the stack
# version so we don't need to do this too often.

if [ -z $MIN_STACK_VER ]; then
  echo "Missing MIN_STACK_VER"
  exit 1
fi

if [ -z "$CACHED_MSV_FILE" ]; then
  echo "Missing CACHED_MSV_FILE"
  exit 1
fi

if [ -f "$CACHED_MSV_FILE" ]; then
  # Fast exit
  CACHED=$(cat "$CACHED_MSV_FILE")
  if [ $CACHED = $MIN_STACK_VER ]; then
    exit 0
  fi
fi

BIN=stack DOWNLOAD_LOCATION="https://www.haskellstack.org/" "$SHELL" scripts/check_binary.sh
RET=$?
if [ $RET -ne 0 ]; then
  exit 1
fi

CURRENT_VER=$(stack --numeric-version)

MSV_SPACE=$(echo $MIN_STACK_VER | tr "." " ")
MSV_LEN=$(echo $MSV_SPACE | wc -w)
CV_SPACE=$(echo $CURRENT_VER | tr "." " ")
CV_LEN=$(echo $CV_SPACE | wc -w)

VALID_VERSION=yes
for i in $(seq 1 $MSV_LEN); do
  if [ $i -gt $CV_LEN ]; then
    VALID_VERSION=no
    break
  fi

  CVV=$(echo $CV_SPACE | cut -d" " -f$i)
  MSVV=$(echo $MSV_SPACE | cut -d" " -f$i)
  if [ $CVV -ne $MSVV ]; then
    if [ $CVV -lt $MSVV ]; then
      VALID_VERSION=no
    fi
    break
  fi
done

if [ $VALID_VERSION = "yes" ]; then
  echo $MIN_STACK_VER > "$CACHED_MSV_FILE"
  exit 0
else
  echo "It appears your version of stack is out of date."
  echo "Minimum required version: $MIN_STACK_VER"
  echo "Current version: $CURRENT_VER"
  echo "Considering running \`stack upgrade\` to update stack."
  exit 1
fi
