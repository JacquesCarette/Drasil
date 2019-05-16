#!/bin/sh
if [ $# -gt 0 ]; then
  printout="$1"
else
  printout="no"
fi
errors="no"
exitval=0

for logfile in $LOG_FOLDER*$LOG_SUFFIX; do
  if [ -s $logfile ]; then
    echo "-------------------------------------------"
    echo "- $logfile IS NOT EMPTY -- DIFFERENCE"
    echo "- BETWEEN GENERATED AND STABLE OUTPUT FOUND"
    echo "-------------------------------------------"
    errors="yes"
    if [ "$printout" = "yes" ]; then
      echo "- $logfile"
      echo "-------------------------------------------"
      cat "$logfile"
      echo "-------------------------------------------"
    fi
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
