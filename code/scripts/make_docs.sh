DOCS_FOLDER=docs/
FULL_DOCS_FOLDER=docs/full/

# Clean workspace
rm -rf $DOCS_FOLDER
mkdir -p $FULL_DOCS_FOLDER

# Get location of buildable docs location

# Build full variant
stack haddock --no-haddock-deps --no-haddock-hyperlink-source --haddock-arguments "--show-all"

# Get doc folder
DOCS_LOC="$(stack path --local-install-root)/doc"

# Copy over full docs
cp -rf "$DOCS_LOC/." "$FULL_DOCS_FOLDER"

# Clean up build artifacts
# Unfortunately, this is needed because haddock requires a compilation
stack clean

# Build small variant
stack haddock --no-haddock-deps --no-haddock-hyperlink-source

# Find new docs folder
DOCS_LOC="$(stack path --local-install-root)/doc"

# Copy over small docs
cp -rf "$DOCS_LOC/." "$DOCS_FOLDER"


