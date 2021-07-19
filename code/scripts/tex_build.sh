# Build LaTeX files

if [ -z "$EDIR" ]; then
  echo "Missing EDIR."
  exit 1
fi
if [ -z "$MAKE" ]; then
  echo "Missing MAKE."
  exit 1
fi
if [ -z "$SUMMARIZE_TEX" ]; then
  echo "Missing SUMMARIZE_TEX."
  exit 1
fi

if [ "$SUMMARIZE_TEX" = "yes" ]; then
  IMODE=batchmode
  BIFLAGS=-terse
else
  IMODE=nonstopmode
fi

GEN_NAME_SUFFIX=_SRS

cd "$BUILD_FOLDER$EDIR"/SRS/
"$MAKE" TEXFLAGS="-interaction=$IMODE --shell-escape" BIBTEXFLAGS="$BIFLAGS"
RET=$?

if [ "$SUMMARIZE_TEX" = "yes" ]; then
  echo "\n\n\033[0;33m$EDIR TeX Summary\033[0m:"
  if [ "$RET" -eq 0 ]; then
    # Approximate error gathering from TeX logs.
    cat "$EDIR$GEN_NAME_SUFFIX".log | grep -E "erfull|Warning"
    cat "$EDIR$GEN_NAME_SUFFIX".blg | grep -B3 -E "Error"
    BIBERRS=$(cat "$EDIR$GEN_NAME_SUFFIX".blg | grep -E "Error" | wc -l)
    if [ "$BIBERRS" -gt 0 ]; then
      # This conditional is due to the current way TeX makefiles are generated.
      # BibTeX return value is ignored (specifically with HGHC having no
      # references). 
      RET=1
    fi
  else
    # Most "useful" output is the last run of lualatex. Only print that.
    cat "$EDIR$GEN_NAME_SUFFIX".log
  fi
  echo ""
fi
exit $RET
