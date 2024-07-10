#!/usr/bin/env bash

# Check if a file path is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 path_to_mdBook_project"
    exit 1
fi

# Change to the mdBook project directory
cd "$1"

# Find any CSV file in the directory
csv_file=$(find . -maxdepth 1 -name '*.csv' | head -n 1)

# Check if a CSV file was found
if [ -z "$csv_file" ]; then
    echo "No CSV file found in the directory."
    exit 1
fi

# Read the CSV file line by line
while IFS=, read -r original copy; do
    dest_dir=$(dirname "$copy")
    mkdir -p "$dest_dir"
    cp "$original" "$copy"
done < "$csv_file"
