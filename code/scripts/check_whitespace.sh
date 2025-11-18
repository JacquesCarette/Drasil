#!/usr/bin/env bash

exit_code=0

while IFS= read -r -d '' f
do
    if [ -n "$(tail -c 1 <"$f")" ] ; then
        echo "MISSING TRAILING NEWLINE CHARACTER: $f"

        exit_code=1
    fi

    bad_lines=$(grep -nE "\s+$" "$f")
    if [ -n "$bad_lines" ] ; then
        echo "$f CONTAINS PER-LINE TRAILING WHITESPACE:"
        echo "$bad_lines"

        exit_code=1
    fi
done < <(find . -type f -name "*.hs" -print0)

exit $exit_code
