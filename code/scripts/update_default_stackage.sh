#!/usr/bin/env bash

update_files() {
    # $search and $replace use SED regex syntax!
    local dir="$1"
    local targets="$2"
    local search="$3"
    local replace="$4"

    find "$dir" -type f -name "$targets" | while read file; do
        sed -i -E "s|$search|$replace|g" "$file"
        echo "Updated: $file"
    done
}

fetch_hlint_version() {
    local redirect_url=$(curl -Ls -o /dev/null -w %{url_effective} "https://www.stackage.org/lts-$1/package/hlint")
    local hlint_version=$(echo "$redirect_url" | grep -oE "hlint-[0-9]+\.[0-9]+\.[0-9]+" | sed 's/hlint-//')

    echo "$hlint_version"
}

if [ -z "$1" ]; then
    echo "Error: Missing target LTS version."
    echo "Usage: $0 <lts-X>"
    echo "Example: $0 20.26"
    exit 1
fi

# Validate working directory name
current_dir=$(basename "$PWD")
if [ "$current_dir" != "code" ]; then
    echo "Error: Script only works if working directory is 'code'."
    exit 1
fi

# Update stack projects
echo "Updating 'stack.yaml' files..."
update_files "." "stack.yaml" "^resolver: lts-[0-9]+\.[0-9]+$" "resolver: lts-$1"
echo "ok"

# Update Haskell scripts
echo "Updating '.hs' scripts built with Stack..."
update_files "scripts/" "*.hs" "--resolver lts-[0-9]+\.[0-9]+" "--resolver lts-$1"
echo "ok"

# Fetch and display hlint version
echo "Fetching HLint version..."
hlint_version=$(fetch_hlint_version "$1")
if [ -z "$hlint_version" ]; then
    echo "Error: Failed to find HLint version using Stackage package list, please update HLint version manually."
    return 1
fi
echo "Found hlint version: $hlint_version"

# Update HLint workflow
echo "Updating HLint workflow..."
update_files "../.github/workflows/" "Lint.yaml" "version: '[0-9\.]+' \# HLINT VERSION TIED TO CURRENT TARGET \(LTS-[0-9]+\.[0-9]+\)" "version: '$hlint_version' # HLINT VERSION TIED TO CURRENT TARGET (LTS-$1)"
echo "ok"

echo "You should be good to go. However, please look over:"
echo "- **/stack.yaml"
echo "- scripts/**.hs"
echo "- ../.github/workflows/Lint.yaml"
