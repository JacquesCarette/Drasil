#!/usr/bin/env bash

exit_code=0

for f in $(find . -type f -name "*.hs")
do
    if [ ! -z "$(tail -c 1 <"$f")" ] ; then
        echo "MISSING TRAILING NEWLINE CHARACTER: $f"
        exit_code=1
    fi
done

for f in $(find . -type f -name "*.hs")
do
    bad_lines=$(grep -nE "\s+$" "$f")
    if [ ! -z "$bad_lines" ] ; then
        echo "$f CONTAINS PER-LINE TRAILING WHITESPACE:"
        echo "$bad_lines"

        exit_code=1
    fi
done

exit $exit_code
