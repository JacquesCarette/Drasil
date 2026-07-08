#!/usr/bin/env bash

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

cd "$BUILD_FOLDER$EDIR"/SRS/PDF || exit 1
"$MAKE" TEXFLAGS="-interaction=$IMODE --shell-escape" BIBTEXFLAGS="$BIFLAGS"
RET=$?

if [ "$SUMMARIZE_TEX" = "yes" ]; then
  printf "\n\n\033[0;33m%s TeX Summary\033[0m:" "$EDIR"
  LOG_FILE=$(find . -maxdepth 1 -iname "${EDIR}${GEN_NAME_SUFFIX}.log")
  BLG_FILE=$(find . -maxdepth 1 -iname "${EDIR}${GEN_NAME_SUFFIX}.blg")
  if [ "$RET" -eq 0 ]; then
    # Approximate error gathering from TeX logs.
    if [ -n "$LOG_FILE" ] && [ -f "$LOG_FILE" ]; then
      grep -E "erfull|Warning" "$LOG_FILE"
    fi
    BIBERRS=0
    if [ -n "$BLG_FILE" ] && [ -f "$BLG_FILE" ]; then
      grep -B3 -E "Error" "$BLG_FILE"
      BIBERRS=$(grep -c -E "Error" "$BLG_FILE")
    fi
    if [ "$BIBERRS" -gt 0 ]; then
      # This conditional is due to the current way TeX makefiles are generated.
      # BibTeX return value is ignored (specifically with HGHC having no
      # references). 
      RET=1
    fi
  else
    # Most "useful" output is the last run of lualatex. Only print that.
    if [ -n "$LOG_FILE" ] && [ -f "$LOG_FILE" ]; then
      cat "$LOG_FILE"
    fi
  fi
  echo ""
fi
exit $RET
