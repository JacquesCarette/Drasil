if [ -z "$DEPLOY_FOLDER" ]; then
	echo "Need DEPLOY_FOLDER to know where to stage deploy."
	exit 1
fi

if [ -z "$GRAPH_FOLDER" ]; then
	echo "Missing GRAPH_FOLDER."
	exit 1
fi

DOC_DEST=docs/

copy_docs() {
  DOC_DIR=$(stack path | grep local-doc-root | cut -d":" -f2 | sed -e "s/^ //")/
  rm -r "$DOC_DEST" >/dev/null 2>&1  # Printing an error message that a directory doesn't exist isn't the most useful.
  mkdir -p "$DOC_DEST"
  cp -r "$DOC_DIR". "$DOC_DEST"
}

copy_graphs() {
  rm -r "$GRAPH_FOLDER" >/dev/null 2>&1  # Printing an error message that a directory doesn't exist isn't the most useful.
  cp -r ../"$GRAPH_FOLDER". "$GRAPH_FOLDER"
}

cd "$DEPLOY_FOLDER"
copy_docs
copy_graphs
echo "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>Drasil</title></head><body>Missing real index.</body></html>" > index.html