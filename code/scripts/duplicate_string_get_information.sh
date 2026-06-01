#!/bin/bash  

# -r so that Backslash does not act as an escape character, so that escape characters are captured
while read -r  p; do
    echo "$p" >>duplicate_string_summary.txt
    #looking for instances of \\, grep needs to be adjusted for escape characters
    if [[ $p == *"\\"* ]]; then
        #remove the first character (double quote) of the current string
        q="${p:1}"
        #finds instances of the string
        #wc -l counts the lines
        stringinstances=$(grep -rho --exclude-dir=./Drasil/code/stable --include \*.hs "$q" ./Drasil/code | wc -l)
        echo "Number of instances of string: ${stringinstances}">>duplicate_string_summary.txt
        #find number of files containing string, sort by unique, count the output lines
        filescontainstring=$(grep -ro --exclude-dir=./Drasil/code/stable --include \*.hs "$q" ./Drasil/code | sort -u | wc -l)    
        echo "Number of files containing the string: ${filescontainstring}" >>duplicate_string_summary.txt
        #output all unique file locations
        grep -ro --exclude-dir=./Drasil/code/stable --include \*.hs "$q" ./Drasil/code | sort -u >>duplicate_string_summary.txt
    #everything that does not have \\ falls to the else    
    else    
        stringinstances=$(grep -rho --exclude-dir=./Drasil/code/stable --include \*.hs "$p" ./Drasil/code | wc -l)
        echo "Number of instances of string: ${stringinstances}">>duplicate_string_summary.txt
        filescontainstring=$(grep -ro --exclude-dir=./Drasil/code/stable --include \*.hs "$p" ./Drasil/code | sort -u | wc -l)    
        echo "Number of files containing the string: ${filescontainstring}" >>duplicate_string_summary.txt
        grep -ro --exclude-dir=./Drasil/code/stable --include \*.hs "$p" ./Drasil/code | sort -u >>duplicate_string_summary.txt
    fi    
    printf '\n'>>duplicate_string_summary.txt
done <duplicate_strings.txt






