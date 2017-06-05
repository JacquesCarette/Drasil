#include "InputFormat.h"

#include <stdio.h>
#include <stdlib.h>

void get_input(char* filename, InputParameters* params){
  FILE* infile;
  infile = fopen(filename, "r");
  fscanf(infile, "%lf", &params->a);
  fscanf(infile, "%lf", &params->b);
  fscanf(infile, "%lf", &params->t);
  fscanf(infile, "%d", &params->gt);
  fscanf(infile, "%lf", &params->w);
  fscanf(infile, "%lf", &params->tnt);
  fscanf(infile, "%lf", &params->sdx);
  fscanf(infile, "%lf", &params->sdy);
  fscanf(infile, "%lf", &params->sdz);
  fscanf(infile, "%lf", &params->pbtol);
  fclose(infile);
}