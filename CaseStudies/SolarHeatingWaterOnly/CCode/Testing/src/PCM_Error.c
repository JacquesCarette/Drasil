#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "parameters.h"
#include "load_params.h"
#include "PCM_Error.h"
#include "linterp.h"

double PCM_ErrorF(const char Ffile[], const char Cfile[], const char comparator[], struct parameters params){

    FILE * fPointer;
    fPointer = fopen(Ffile, "r");
    int i;
    char c;
    for(i = 0; i < 23; i++){
        do{
            c = fgetc(fPointer);
        }while(c != '\n');
    }
    int sizeOfResults = ceil(params.tfinal / params.tstep);
    double time1[sizeOfResults], tempW1[sizeOfResults], tempP1[sizeOfResults], eW1[sizeOfResults], eP1[sizeOfResults], tNoPCM[sizeOfResults], eNoPCM[sizeOfResults];
    double meltFrac, eTot;
    int iteration;
    int counter1 = 0;
    while(!feof(fPointer)){
        fscanf(fPointer, "%lf %d %lf %lf %lf %lf %lf %lf %lf %lf", &time1[counter1], &iteration, &tempW1[counter1], &tempP1[counter1], &eP1[counter1], &eW1[counter1], &eTot, &meltFrac, &tNoPCM[counter1], &eNoPCM[counter1]);
        counter1++;
    }
    fclose(fPointer);


    FILE * cPointer;
    cPointer = fopen(Cfile, "r");
    int numberLines = 0;
    char singleLine[150];
    while(!feof(cPointer)){
        fgets(singleLine, 150, cPointer);
        numberLines++;
    }

    rewind(cPointer);
    int j;
    char ch;
    for(j = 0; j < 34; j++){
        do{
            ch = fgetc(cPointer);
        }while(ch != '\n');
    }

    int sizeOfResults2 = numberLines - 35;
    double time2[sizeOfResults2], tempW2[sizeOfResults2], tempP2[sizeOfResults2], eW2[sizeOfResults2], eP2[sizeOfResults2];
    double eTot2;
    int counter2= 0;
    while(!feof(cPointer)){
        fscanf(cPointer, "%lf %lf %lf %lf %lf %lf", &time2[counter2], &tempW2[counter2], &tempP2[counter2], &eW2[counter2], &eP2[counter2], &eTot2);
        counter2++;
    }
    fclose(cPointer);

    double error;

    if(strcmp("TWat", comparator) == 0){
        error = errorCalcInterp(time1, time2, tempW1, tempW2, sizeOfResults, sizeOfResults2);
    }
    else if(strcmp("TPCM", comparator) == 0){
        error = errorCalcInterp(time1, time2, tempP1, tempP2, sizeOfResults, sizeOfResults2);
    }
    else if(strcmp("EWat", comparator) == 0){
        error = errorCalcInterp(time1, time2, eW1, eW2, sizeOfResults, sizeOfResults2);
    }
    else if(strcmp("EPCM", comparator) == 0){
        error = errorCalcInterp(time1, time2, eP1, eP2, sizeOfResults, sizeOfResults2);
    }
    else if(strcmp("TWatNoP", comparator) == 0){
        error = errorCalcInterp(time1, time2, tNoPCM, tempW2, sizeOfResults, sizeOfResults2);
    }
    else if(strcmp("EWatNoP", comparator) == 0){
        error = errorCalcInterp(time1, time2, eNoPCM, eW2, sizeOfResults, sizeOfResults2);
    }
    else{
        error = 1;
    }

    return error;
}

double PCM_ErrorM(const char Mfile[], const char Cfile[], const char comparator[], struct parameters params){

    FILE * fPointer;
    fPointer = fopen(Mfile, "r");
    int i;
    char c;
    for(i = 0; i < 36; i++){
        do{
            c = fgetc(fPointer);
        }while(c != '\n');
    }
    int sizeOfResults = ceil(params.tfinal / params.tstep)+1;
    double time1[sizeOfResults], tempW1[sizeOfResults], tempP1[sizeOfResults], eW1[sizeOfResults], eP1[sizeOfResults];
    double eTot;
    int counter1 = 0;
    while(!feof(fPointer)){
        fscanf(fPointer, "%lf %lf %lf %lf %lf %lf", &time1[counter1], &tempW1[counter1], &tempP1[counter1], &eP1[counter1], &eW1[counter1], &eTot);
        counter1++;
    }
    fclose(fPointer);

    int sizeOfResultsM = 1;
    while(1){
        if(time1[sizeOfResultsM] == params.tfinal){
            sizeOfResultsM++;
            break;
        }
        sizeOfResultsM++;
    }


    FILE * cPointer;
    cPointer = fopen(Cfile, "r");
    int numberLines = 0;
    char singleLine[150];
    while(!feof(cPointer)){
        fgets(singleLine, 150, cPointer);
        numberLines++;
    }

    rewind(cPointer);
    int j;
    char ch;
    for(j = 0; j < 34; j++){
        do{
            ch = fgetc(cPointer);
        }while(ch != '\n');
    }
    int sizeOfResults2 = numberLines - 35;
    double time2[sizeOfResults2], tempW2[sizeOfResults2], tempP2[sizeOfResults2], eW2[sizeOfResults2], eP2[sizeOfResults2];
    double eTot2;
    int counter2= 0;
    while(!feof(cPointer)){
        fscanf(cPointer, "%lf %lf %lf %lf %lf %lf", &time2[counter2], &tempW2[counter2], &tempP2[counter2], &eW2[counter2], &eP2[counter2], &eTot2);
        counter2++;
    }
    fclose(cPointer);

    double error;

    if(strcmp("TWat", comparator) == 0){
        error = errorCalcInterp(time1, time2, tempW1, tempW2, sizeOfResultsM, sizeOfResults2);
    }
    else if(strcmp("TPCM", comparator) == 0){
        error = errorCalcInterp(time1, time2, tempP1, tempP2, sizeOfResultsM, sizeOfResults2);
    }
    else if(strcmp("EWat", comparator) == 0){
        error = errorCalcInterp(time1, time2, eW1, eW2, sizeOfResultsM, sizeOfResults2);
    }
    else if(strcmp("EPCM", comparator) == 0){
        error = errorCalcInterp(time1, time2, eP1, eP2, sizeOfResultsM, sizeOfResults2);
    }
    else{
        error = 1;
    }
    return error;
}

double PCM_ErrorC(const char Cfile1[], const char Cfile2[], const char comparator[]){

    FILE * fPointer;
    fPointer = fopen(Cfile1, "r");
    int numberLines1 = 0;
    char singleLine[150];
    while(!feof(fPointer)){
        fgets(singleLine, 150, fPointer);
        numberLines1++;
    }

    rewind(fPointer);
    int i;
    char c;
    for(i = 0; i < 34; i++){
        do{
            c = fgetc(fPointer);
        }while(c != '\n');
    }
    int sizeOfResults1 = numberLines1 - 35;
    double time1[sizeOfResults1], tempW1[sizeOfResults1], tempP1[sizeOfResults1], eW1[sizeOfResults1], eP1[sizeOfResults1];
    double eTot;
    int counter1 = 0;
    while(!feof(fPointer)){
        fscanf(fPointer, "%lf %lf %lf %lf %lf %lf", &time1[counter1], &tempW1[counter1], &tempP1[counter1], &eW1[counter1], &eP1[counter1], &eTot);
        counter1++;
    }
    fclose(fPointer);


    FILE * cPointer;
    cPointer = fopen(Cfile2, "r");
    int numberLines2 = 0;
    char singleLine2[150];
    while(!feof(cPointer)){
        fgets(singleLine2, 150, cPointer);
        numberLines2++;
    }

    rewind(cPointer);
    int j;
    char ch;
    for(j = 0; j < 34; j++){
        do{
            ch = fgetc(cPointer);
        }while(ch != '\n');
    }

    int sizeOfResults2 = numberLines2 - 35;
    double time2[sizeOfResults2], tempW2[sizeOfResults2], tempP2[sizeOfResults2], eW2[sizeOfResults2], eP2[sizeOfResults2];
    double eTot2;
    int counter2= 0;
    while(!feof(cPointer)){
        fscanf(cPointer, "%lf %lf %lf %lf %lf %lf", &time2[counter2], &tempW2[counter2], &tempP2[counter2], &eW2[counter2], &eP2[counter2], &eTot2);
        counter2++;
    }
    fclose(cPointer);

    double error;

    if(strcmp("TWat", comparator) == 0){
        error = errorCalcInterp(time1, time2, tempW1, tempW2, sizeOfResults1, sizeOfResults2);
    }
    else if(strcmp("TPCM", comparator) == 0){
        error = errorCalcInterp(time1, time2, tempP1, tempP2, sizeOfResults1, sizeOfResults2);
    }
    else if(strcmp("EWat", comparator) == 0){
        error = errorCalcInterp(time1, time2, eW1, eW2, sizeOfResults1, sizeOfResults2);
    }
    else if(strcmp("EPCM", comparator) == 0){
        error = errorCalcInterp(time1, time2, eP1, eP2, sizeOfResults1, sizeOfResults2);
    }
    else{
        error = 1;
    }
    return error;
}

double errorCalcInterp(double time1[], double time2[], double result1[], double result2[], int sizeOfResultsM, int sizeOfResults){
    double time2Shifted[sizeOfResultsM];
    double result2Interpolated[sizeOfResultsM];
    int i;
    int j = 1;
    for(i = 0; i < sizeOfResultsM; i++){
        time2Shifted[i] = time1[i];
        while(j < sizeOfResults){
                if(time2[j] >= time2Shifted[i]){
                    result2Interpolated[i] = linterp(time2[j-1], time2[j], result2[j-1], result2[j], time2Shifted[i]);
                    break;
                }
                j++;
        }
    }

    double delta[sizeOfResultsM];
    double deltaSumSquares = 0;
    double resultSumSquares = 0;
    int k;
    for(k = 0; k < sizeOfResultsM; k++){
        delta[k] = fabs(result1[k] - result2Interpolated[k]);
        deltaSumSquares += pow(delta[k], 2);
        resultSumSquares += pow(result1[k], 2);
    }
    double deltaNorm, resultNorm;
    deltaNorm = sqrt(deltaSumSquares);
    resultNorm = sqrt(resultSumSquares);
    double error;
    error = deltaNorm / resultNorm;
    return error;
}

