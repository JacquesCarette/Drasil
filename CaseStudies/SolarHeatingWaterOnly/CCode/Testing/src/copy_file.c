#include <stdio.h>
#include "copy_file.h"

void copy_file(const char pathToFile[], const char newFilename[]){
    FILE * origF;
    origF = fopen(pathToFile, "r");
    FILE * newF;
    newF = fopen(newFilename, "w");
    char ch;
    while(!feof(origF)){
        ch = fgetc(origF);
        fputc(ch, newF);
    }
    fclose(origF);
    fclose(newF);
    return;
}
