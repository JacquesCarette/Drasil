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
stack haddock --haddock-arguments "--show-all" --ghc-options="$GHC_FLAGS"

# Get doc folder
DOCS_LOC="$(stack path --local-install-root)/doc"

# Copy over full docs
cp -rf "$DOCS_LOC/." "$FULL_DOCS_FOLDER"

# Clean up build artifacts
# Unfortunately, this is needed because haddock requires a compilation
stack clean

# Build small variant
stack haddock --ghc-options="$GHC_FLAGS" 

# Find new docs folder
DOCS_LOC="$(stack path --local-install-root)/doc"

# Copy over small docs
cp -rf "$DOCS_LOC/." "$DOCS_FOLDER"

echo "-------------------------------------------"
echo "- Docs: ./$DOCS_FOLDER"
echo "- Full docs: ./$FULL_DOCS_FOLDER"
echo "-------------------------------------------"
