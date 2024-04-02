#!/usr/bin/env bash

update_files() {
    local dir="$1"
    local targets="$2"
    local search="$3"
    local replace="$4"

    find "$dir" -type f -name "$targets" | while read file; do
        sed -i -E "s|$search|$replace|g" "$file"
        echo "Updated: $file"
    done
}

if [ -z "$1" ]; then
    echo "Usage: $0 <lts-X>"
    echo "Example: $0 20.26"
    exit 1
fi

# Update stack projects
update_files "." "stack.yaml" "^resolver: lts-[0-9]+\.[0-9]+$" "resolver: lts-$1"

# Update Haskell scripts
update_files "scripts/" "*.hs" "--resolver lts-[0-9]+\.[0-9]+" "--resolver lts-$1"
