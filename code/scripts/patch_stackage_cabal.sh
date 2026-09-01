#!/usr/bin/bash

VERSION=lts-24.51

curl https://www.stackage.org/$VERSION/cabal.config -o stackage-lts-$VERSION.config

INPUT=stackage-$VERSION.config
OUTPUT=stackage-lts-$VERSION-patched.config

cp $INPUT $OUTPUT

# Remove 'template'
sed -i '/ template /d' $OUTPUT
