#!/usr/bin/env bash

# Since Drasil hides many modules in the built libraries, the normal Haddock docs
# become a bit difficult to read or appear to be missing components when read.
#
# This script creates 2 variants of Haddock documentation for all Drasil packages:
#   1. With all modules exposed.
#   2. As-is.

if [ -z "$DOCS_FOLDER" ]; then
  echo "Missing DOCS_FOLDER environment variable, defaulting to 'docs/'"
  DOCS_FOLDER="docs/"
fi

FULL_DOCS_FOLDER="${DOCS_FOLDER}full/"

# Clean workspace
rm -rf "$DOCS_FOLDER"
mkdir -p "$FULL_DOCS_FOLDER"

# Get location of buildable docs location

# Build full variant
if stack haddock --haddock-arguments "--show-all" --ghc-options="$GHC_FLAGS"; then
  echo "Successfully created fully exposed variant of Haddock docs!"
else
  echo "Fully exposed Haddock build failed!"
  exit 1
fi

if [ "$FULL" -ne "1" ]; then
  echo "Haddock docs build (with strictly the intent to test Haddock generation) complete. Exiting."
  exit 0
fi

# Get doc folder
DOCS_LOC="$(stack path --local-install-root)/doc"

# Copy over full docs
cp -rf "$DOCS_LOC/." "$FULL_DOCS_FOLDER"

# Clean up build artifacts
# Unfortunately, this is needed because haddock requires a compilation
stack clean

# Build small variant
if stack haddock --ghc-options="$GHC_FLAGS"; then
  echo "Successfully created normal Haddock docs!"
else
  echo "Normal Haddock build failed!"
  exit 1
fi

# Find new docs folder
DOCS_LOC="$(stack path --local-install-root)/doc"

# Copy over small docs
cp -rf "$DOCS_LOC/." "$DOCS_FOLDER"

echo "-------------------------------------------"
echo "- Docs: ./$DOCS_FOLDER"
echo "- Full docs: ./$FULL_DOCS_FOLDER"
echo "-------------------------------------------"
