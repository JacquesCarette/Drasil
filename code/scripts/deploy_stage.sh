if [ -z "$DEPLOY_FOLDER" ]; then
	echo "Need DEPLOY_FOLDER to know where to stage deploy."
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

if [ -z "$DEPLOY_CODE_PATH_KV_SEP" ]; then
	echo "Missing DEPLOY_CODE_PATH_KV_SEP."
	exit 1
fi

if [ -z "$MAKE" ]; then
	echo "Missing MAKE. Did you invoke this with \`make deploy(_lite)\`?"
	exit 1
fi

DOC_DEST=docs/
SRS_DEST=srs/
EXAMPLE_DEST=examples/
CUR_DIR="$PWD/"


copy_docs() {
  # The doc directory can be in two locations (as of Stack 2.1.1) depending on which arguments are
  # passed to `stack haddock`. The first directory is when no arguments are specified. The second
  # is used when `--no-haddock-deps` is passed as an argument.
  DOC_DIR=$(cd "$CUR_DIR" && stack path --local-doc-root)/
  if [ ! -d "$DOC_DIR" ]; then
    DOC_DIR=$(cd "$CUR_DIR" && stack path --local-install-root)/doc/
  fi
  rm -r "$DOC_DEST" >/dev/null 2>&1  # Printing an error message that a directory doesn't exist isn't the most useful.
  mkdir -p "$DOC_DEST"
  cp -r "$DOC_DIR". "$DOC_DEST"
}

copy_datafiles() {
	echo "FIXME: Drasil should copy needed images and resources to the appropriate output directory to avoid needing the entirety of datafiles (for HTML)."
	rm -r datafiles  >/dev/null 2>&1  # Printing an error message that a directory doesn't exist isn't the most useful.
	mkdir -p datafiles
	cp -r "$CUR_DIR"datafiles/. datafiles/
}

copy_graphs() {
  rm -r "$GRAPH_FOLDER" >/dev/null 2>&1  # Printing an error message that a directory doesn't exist isn't the most useful.
  cp -r "$CUR_DIR$GRAPH_FOLDER". "$GRAPH_FOLDER"
}

copy_examples() {
	rm -r "$EXAMPLE_DEST"
	for example in "$CUR_DIR$BUILD_FOLDER"*; do
		example_name=$(basename "$example")
		mkdir -p "$EXAMPLE_DEST$example_name/$SRS_DEST"
		if [ -d "$example/"SRS ]; then
			cp "$example/"SRS/*.pdf "$EXAMPLE_DEST$example_name/$SRS_DEST"
		fi
		if [ -d "$example/"Website/ ]; then
			cp -r "$example/"Website/. "$EXAMPLE_DEST$example_name/$SRS_DEST"
		fi
		if [ -d "$example/"src ]; then
			# We don't expose code in deploy. It's more conveneient to link to GitHub's directory
			# We place a stub file which Hakyll will replace.
			echo $(cd "$CUR_DIR" && "$MAKE" deploy_code_path | grep "$example_name" | cut -d"$DEPLOY_CODE_PATH_KV_SEP" -f 2-) > "$EXAMPLE_DEST$example_name/src"
		fi
	done
}

build_website() {
	cd "$CUR_DIR"website
	make DEPLOY_FOLDER="$CUR_DIR$DEPLOY_FOLDER" DOCS_FOLDER="$DOC_DEST" EXAMPLES_FOLDER="$EXAMPLE_DEST" \
	SRS_FOLDER_FRAG="$SRS_DEST" GRAPH_FOLDER="$GRAPH_FOLDER"
	RET=$?
	if [ $RET != 0 ]; then
		echo "Build Failed. Bailing."
		exit 1
	fi
	cd "$CUR_DIR$DEPLOY_FOLDER"
	cp -r "$CUR_DIR"website/_site/. .

	# src stubs were consumed by site generator; safe to delete those.
	rm "$EXAMPLE_DEST"*/src
}


cd "$DEPLOY_FOLDER"
copy_docs
copy_graphs
copy_datafiles
copy_examples
build_website
cd "$CUR_DIR"
