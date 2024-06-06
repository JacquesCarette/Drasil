This page will document interesting design notes about GOOL and Drasil's code generator. 

- GOOL's `int` type maps to 32-bit signed integers in most target languages, but in Python 3 the default integer type has unlimited precision, and this is used instead.