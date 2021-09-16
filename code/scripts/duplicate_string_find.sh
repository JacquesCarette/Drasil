#!/bin/bash  

#grep -rhEo (recursive, no filename, extended regular expressions, prints only matching parts
#excludes stable, only includes .hs files
#cuts outer double quotations
#first awk searches for instances that appear 3 or more times
#second awk removes instances that have double quotations anywhere by the ends
#last two seds add back the double quotations
#addition and removal of quotations necessary to deal with situations like "" = ""

grep -rhEo --exclude-dir=./Drasil/code/stable --include \*.hs '".{5,100}"' ./Drasil/code | cut -c 2- | rev | cut -c 2- | rev | awk '++A[$0]==3,1000' | awk '/^"/ || /"$/ || !/"/' | sed 's/^/"/' | sed 's/$/"/' > duplicate_strings.txt





