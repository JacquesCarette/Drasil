#include <stdio.h>
#include <stdlib.h>
#include "calc.h"

int main(int argc, char* argv[])
{
    if(argc != 2){
        printf("Usage:  hghc infile\n");
        exit(1);
    }

    FILE *inFile = fopen(argv[1], "r");
    if(!inFile){
        fprintf(stderr, "Could not open file \"%s\"\n", argv[1]);
        exit(2);
    }

    FILE *outFile = fopen("outfile", "w+");
    if(!outFile){
        fprintf(stderr, "Could not create output file.\n", argv[1]);
        exit(3);
    }

    while(!feof(inFile)){
        double k_c, h_p, h_b, tau_c;
        fscanf(inFile, "%lf %lf %lf %lf", &k_c, &h_p, &h_b, &tau_c);
        double h_g = calc_h_g(k_c, h_p, tau_c);
        if(calc_get_error_code() == 1)
            fprintf(outFile, "[ERR: divide by zero] ");
        else
            fprintf(outFile, "%lf ", h_g);
        double h_c = calc_h_c(k_c, h_b, tau_c);
        if(calc_get_error_code() == 1)
            fprintf(outFile, "[ERR: divide by zero]\n");
        else
            fprintf(outFile, "%lf\n", h_c);
    }

    fclose(inFile);
    fclose(outFile);

    return 0;
}
