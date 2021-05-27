#!/bin/bash
for source in ./*.hs; do
    #haskell-formatter must be installed to run this script.
    haskell-formatter --input=$source --output=$source -f
done