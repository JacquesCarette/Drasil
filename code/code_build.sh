if [ -z "$EDIR" ]; then
	echo "Missing EDIR.";
	exit 1;
fi;
if [ -z "$MAKE" ]; then
	echo "Missing MAKE.";
	exit 1;
fi;

RET=0;

if [ -d "./build/$EDIR/src" ]; then
	cd "./build/$EDIR/src";
	for d in */; do
		cd "$d";
		$MAKE;
		RET=$(( $RET || $? ));
		cd ../;
	done; \
fi
exit $RET;
