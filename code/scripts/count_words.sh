#!/usr/bin/env bash

if [[ "$1" == "-h" ]]; then
  echo "Usage: sh count_words.sh [-h] \"some file\""
elif [[ -z "$1" ]]; then
  echo "missing file argument to read from"
  exit 1
elif [[ -r "$1" ]]; then
  tr " " "\n" < "$1" | sort | uniq -c
else
  echo "provided file is not readable"
  exit 1
fi
