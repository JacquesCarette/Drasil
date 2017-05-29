#ifndef INPUTPARAMETERS_H
#define INPUTPARAMETERS_H

typedef struct InputParameters {
  double a;
  double b;
  double t;
  int gt;
  double w;
  double tnt;
  double sdx;
  double sdy;
  double sdz;
  double pbtol;
  double asprat;
  double sd;
  double h;
  double gtf;
  double ldf;
  double wtnt;
} InputParameters;

InputParameters* new_InputParameters();
void delete_InputParameters(InputParameters* params);

#endif /* INPUTPARAMETERS_H */