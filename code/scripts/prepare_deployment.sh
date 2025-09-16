#!/usr/bin/env bash

# Get all files ready for deploy. Checks if website files exist,
# and then copies each file to a deploy folder.

if [ -z "$DEPLOY_FOLDER" ]; then
  echo "Need DEPLOY_FOLDER to know where to stage deploy."
  exit 1
fi

if [ -z "$WEBSITE_FOLDER" ]; then
  echo "Missing WEBSITE_FOLDER. Run make website."
  exit 1
fi

if [ -z "$BUILD_FOLDER" ]; then
  echo "Missing BUILD_FOLDER."
  exit 1
fi

if [ -z "$GRAPH_FOLDER" ]; then
  echo "Missing GRAPH_FOLDER."
  exit 1
fi

if [ -z "$TRACEY_GRAPHS_FOLDER" ]; then
  echo "Missing TRACEY_GRAPHS_FOLDER."
  exit 1
fi

if [ -z "$MULTI_SRC_DIRS" ]; then
  echo "Missing MULTI_SRC_DIRS."
  exit 1
fi

if [ -z "$EXAMPLE_DIRS" ]; then
  echo "Missing EXAMPLE_DIRS."
  exit 1
fi

if [ -z "$ANALYSIS_FOLDER" ]; then
  echo "Missing ANALYSIS_FOLDER."
  exit 1
fi

if [ -z "$DEPLOY_CODE_PATH_KV_SEP" ]; then
  echo "Missing DEPLOY_CODE_PATH_KV_SEP."
  exit 1
fi

if [ -z "$MAKE" ]; then
  echo "Missing MAKE. Did you invoke this with \`make deploy(_lite)\`?"
  exit 1
fi

DOC_DEST=docs/
SRS_DEST=SRS
DOX_DEST=doxygen/
EXAMPLE_DEST=examples/
CUR_DIR="$PWD/"

if [ ! -d "$DOC_DEST" ]; then
  echo "Missing $DOC_DEST folder artifacts."
  exit 1
fi

copy_docs() {
  rm -rf "$DOC_DEST"
  cp -r "$CUR_DIR$DOC_DEST" "$DOC_DEST"
}

copy_datafiles() {
  echo "FIXME: Drasil should copy needed images and resources to the appropriate output directory to avoid needing the entirety of datafiles (for HTML)."
  rm -rf datafiles
  mkdir -p datafiles
  cp -r "$CUR_DIR/datafiles/." datafiles/
}

copy_graphs() {
  rm -rf "$GRAPH_FOLDER"
  cp -r "$CUR_DIR$GRAPH_FOLDER." "$GRAPH_FOLDER"
}

copy_examples() {
  if [ -d "$EXAMPLE_DEST" ]; then
    rm -r "$EXAMPLE_DEST"
  fi
  for example in "$CUR_DIR$BUILD_FOLDER"*; do
    example_name=$(basename "$example")
    # Only copy actual examples
    if [[ "$EXAMPLE_DIRS" == *"$example_name"* ]]; then
      target_srs_dir="./$EXAMPLE_DEST$example_name/$SRS_DEST"
      mkdir -p "$target_srs_dir/PDF"
      mkdir -p "$target_srs_dir/HTML"
      mkdir -p "$target_srs_dir/mdBook"
      mkdir -p "$target_srs_dir/Jupyter"
      if [ -d "$example/SRS/PDF" ]; then
        cp "$example/SRS/PDF/"*.pdf "$target_srs_dir/PDF"
      fi
      if [ -d "$example/SRS/HTML" ]; then
        cp -r "$example/SRS/HTML/." "$target_srs_dir/HTML"
      fi
      if [ -d "$example/SRS/mdBook" ]; then
        cp -r "$example/SRS/mdBook/book/" "$target_srs_dir/mdBook"
      fi
      if [ -d "$example/SRS/Jupyter" ]; then
        cp -r "$example/SRS/Jupyter/"*.html "$target_srs_dir/Jupyter"
      fi
      if [ -d "$example/src" ]; then
        mkdir -p "$EXAMPLE_DEST$example_name/$DOX_DEST"
        for lang in "$example/src/"*; do
          lang_name=$(basename "$lang")
          if [ "$lang_name" != "swift" ]; then
            mkdir -p "$EXAMPLE_DEST$example_name/$DOX_DEST$lang_name"
            cp -r "$lang/html/." "$EXAMPLE_DEST$example_name/$DOX_DEST$lang_name/"
          fi
        done
        src_stub
      fi
      # For examples with multiple versions, directory structure is different
      if [[ "$MULTI_SRC_DIRS" == *"$example_name"* ]]; then
        for v in "$example/$example_name"*/; do
          v_name=$(basename "$v")
          mkdir -p "$EXAMPLE_DEST$example_name/$DOX_DEST$v_name/"
          for lang in "$v/"src/*; do
            lang_name=$(basename "$lang")
            if [ "$lang_name" != "swift" ]; then
              mkdir -p "$EXAMPLE_DEST$example_name/$DOX_DEST$v_name/$lang_name"
              cp -r "$lang/html/." "$EXAMPLE_DEST$example_name/$DOX_DEST$v_name/$lang_name/"
            fi
          done
        done
        src_stub
      fi
    fi
  done
}

src_stub() {
  # We don't expose code in deploy. It's more convenient to link to GitHub's directory
  # We place a stub file which Hakyll will replace.
  REL_PATH=$(cd "$CUR_DIR" && "$MAKE" deploy_code_path | grep "$example_name" | cut -d"$DEPLOY_CODE_PATH_KV_SEP" -f 2-)
  # On a real deploy, `deploy` folder is itself a git repo, thus we need to ensure the path lookup is in the outer Drasil repo.
  for p in $REL_PATH; do
    find "$(cd "$CUR_DIR" && git rev-parse --show-toplevel)" -name "$p*" -type d -print0 | rev | cut -d/ -f2 | rev | tr '\0' '\n' | xargs -0 printf "$p%s\n" >> "$EXAMPLE_DEST$example_name/src"
  done
}

copy_images() {
  rm -rf "$CUR_DIR/deploy/images"
  mkdir -p "$CUR_DIR/deploy/images"
  cp -r "$CUR_DIR/drasil-website/WebInfo/images/" "$CUR_DIR/deploy"
}

copy_analysis() {
  rm -rf "$ANALYSIS_FOLDER"
  cp -r "$CUR_DIR$ANALYSIS_FOLDER". "$ANALYSIS_FOLDER"
}

copy_traceygraphs() {
  rm -rf "$TRACEY_GRAPHS_FOLDER"
  cp -r "$CUR_DIR$TRACEY_GRAPHS_FOLDER". "$TRACEY_GRAPHS_FOLDER"
}

copy_website() {
  cd "$CUR_DIR$DEPLOY_FOLDER" || exit 1
  cp -r "$CUR_DIR$WEBSITE_FOLDER". .

  # src stubs were consumed by site generator; safe to delete those.
  rm "$EXAMPLE_DEST"*/src
}


cd "$DEPLOY_FOLDER" || exit 1
copy_docs
copy_graphs
copy_datafiles
copy_examples
copy_images
copy_analysis
copy_traceygraphs
copy_website
cd "$CUR_DIR" || exit 1
