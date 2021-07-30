#!/usr/bin/env bash

# Check if a command exists on the machine, or suggest a way to obtain the command.
# Works for general commands, but exclusively used for binary checking for Drasil.

if [ -z "$BIN" ]; then
  echo "Missing BIN"
  exit 1
fi

if [ -z "$DOWNLOAD_LOCATION" ]; then
  echo "Missing DOWNLOAD_LOCATION"
  exit 1
fi

# `command -v X` finds the command that would be run when typing `X` into the shell.
command -v "$BIN" > /dev/null

RET=$?  # "command" returns 1 if binary not found
if [ $RET -eq 1 ]; then
  echo "Couldn't find \`$BIN\`."
  echo "It can be installed from $DOWNLOAD_LOCATION"
  exit 1
fi
