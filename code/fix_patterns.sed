# Fix function return patterns - success cases should be Right
s/-> Left \([A-Z][a-zA-Z0-9\.]*\)/-> Right \1/g
s/then Left \([A-Z][a-zA-Z0-9\.]*\)/then Right \1/g

# Fix function return patterns - error cases should be Left  
s/-> Right \$/-> Left \$/g
s/else Right \$/else Left \$/g

# Fix case pattern matching - success should be Right, errors should be Left
s/(Left \([a-z][a-zA-Z0-9]*\), Left \([a-z][a-zA-Z0-9]*\))/(Right \1, Right \2)/g
s/(Left \([a-z][a-zA-Z0-9]*\), Right \([a-z][a-zA-Z0-9]*\))/(Right \1, Left \2)/g
s/(Right \([a-z][a-zA-Z0-9]*\), Left \([a-z][a-zA-Z0-9]*\))/(Left \1, Right \2)/g
s/(Right \([a-z][a-zA-Z0-9]*\), Right \([a-z][a-zA-Z0-9]*\))/(Left \1, Left \2)/g

# Fix single variable patterns
s/Left \([a-z][a-zA-Z0-9]*\) ->/Right \1 ->/g
s/Right \([a-z][a-zA-Z0-9]*\) ->/Left \1 ->/g
