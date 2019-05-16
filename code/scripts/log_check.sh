#!/bin/sh
if [ -z "$LOG_FOLDER" ]; then
  echo "Missing LOG_FOLDER."
  exit 1
fi
if [ -z "$LOG_SUFFIX" ]; then
  echo "Missing LOG_SUFFIX."
  exit 1
fi
if [ -z "$NOISY" ]; then
  echo "Missing NOISY."
  exit 1
fi

errors="no"
exitval=0

for logfile in $LOG_FOLDER*$LOG_SUFFIX; do
  if [ -s "$logfile" ]; then
    echo "-------------------------------------------"
    echo "- $logfile IS NOT EMPTY -- DIFFERENCE"
    echo "- BETWEEN GENERATED AND STABLE OUTPUT FOUND"
    echo "-------------------------------------------"
    errors="yes"
    if [ "$NOISY" = "yes" ]; then
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
