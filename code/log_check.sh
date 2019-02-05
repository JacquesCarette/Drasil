#!/bin/sh
SWHS_PREF="SWHS"
TINY_PREF="TINY"
SSP_PREF="SSP"
GLASS_PREF="GlassBR"
GAME_PREF="Chipmunk"
NoPCM_PREF="NoPCM"
log="_log.log"
#
errors="no"
exitval=0
#

EXAMPLES="$SWHS_PREF $TINY_PREF $SSP_PREF $GLASS_PREF $GAME_PREF $NoPCM_PREF"

for e in $EXAMPLES; do
  logfile=$e$log
  if [ -s $logfile ]; then
    echo "-------------------------------------------"
    echo "- $logfile IS NOT EMPTY -- DIFFERENCE"
    echo "- BETWEEN GENERATED AND STABLE OUTPUT FOUND"
    echo "-------------------------------------------"
    errors="yes"
  fi
done


if [ "$errors" = "no" ]; then
  echo "-------------------------------------------"
  echo "- GENERATED OUTPUT MATCHES STABLE VERSION -"
  echo "-------------------------------------------"
else
  echo "-------------------------------------------"
  echo "- ERROR IN GENERATED OUTPUT SEE ABOVE FOR -"
  echo "-             MORE DETAILS                -"
  echo "-------------------------------------------"
  exitval=1
fi

exit $exitval
