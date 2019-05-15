if [ -z "$EDIR" ]; then
	echo "Missing EDIR.";
	exit 1;
fi;
if [ -z "$EPREF" ]; then
	echo "Missing EPREF.";
	exit 1;
fi;
if [ -z "$MAKE" ]; then
	echo "Missing MAKE.";
	exit 1;
fi;
if [ -z "$SUMMARIZE_TEX" ]; then
	echo "Missing SUMMARIZE_TEX.";
	exit 1;
fi;

if [ "$SUMMARIZE_TEX" = "yes" ]; then
	IMODE=batchmode;
	BIFLAGS=-terse;
else
	IMODE=nonstopmode;
fi;

cd ./build/"$EDIR"/SRS/;
$MAKE TEXFLAGS=--interaction="$IMODE" BIBTEXFLAGS="$BIFLAGS";
RET=$?;

if [ "$SUMMARIZE_TEX" = "yes" ]; then
	echo "\n\n\033[0;33m$EDIR TeX Summary\033[0m:";
	if [ "$RET" -eq 0 ]; then
		# Approximate error gathering from TeX logs.
		cat "$EPREF"SRS.log | grep -E "erfull|Warning";
		cat "$EPREF"SRS.blg | grep -B3 -E "Error";
		BIBERRS=$(cat "$EPREF"SRS.blg | grep -E "Error" | wc -l);
		if [ "$BIBERRS" -gt 0 ]; then
			# This conditional is due to the current way TeX makefiles are generated.
			# BibTeX return value is ignored (specifically with Tiny having no
			# references). 
			RET=1;
		fi;
	else
		# Most "useful" output is the last run of lualatex. Only print that.
		cat "$EPREF"SRS.log;
	fi;
	echo "";
fi;
exit $RET;