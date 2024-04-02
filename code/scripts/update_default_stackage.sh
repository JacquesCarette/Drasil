#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Usage: $0 <lts-X>"
    echo "Example: $0 20.26"
    exit 1
fi

search="^resolver: lts-[0-9]+\.[0-9]+$"
replace="resolver: lts-$1"

find "." -type f -name "stack.yaml" | while read file; do
    sed -i -E "s|${search}|${replace}|g" "$file"
    echo "Updated: $file"
done
