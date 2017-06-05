#include "OutputFormat.h"

#include "InputParameters.h"
#include "Constants.h"

#include <stdio.h>

void display_output(char* filename, double q, double j, double q_hat_tol, double pb, double lr, double nfl, int is_safe1, int is_safe2, InputParameters* params) {
    FILE* f = fopen(filename, "w");
    fprintf(f, "E\t%f\n", E);
    fprintf(f, "a\t%f\n", params->a);
    fprintf(f, "asprat\t%f\n", params->asprat);
    fprintf(f, "b\t%f\n", params->b);
    fprintf(f, "gt\t%d\n", params->gt);
    fprintf(f, "gtf\t%f\n", params->gtf);
    fprintf(f, "h\t%f\n", params->h);
    fprintf(f, "K\t%f\n", K);
    fprintf(f, "ldf\t%f\n", params->ldf);
    fprintf(f, "LSF\t%f\n", LSF);
    fprintf(f, "M\t%f\n", M);
    fprintf(f, "pbtol\t%f\n", params->pbtol);
    fprintf(f, "sd\t%f\n", params->sd);
    fprintf(f, "sdx\t%f\n", params->sdx);
    fprintf(f, "sdy\t%f\n", params->sdy);
    fprintf(f, "sdz\t%f\n", params->sdz);
    fprintf(f, "t\t%f\n", params->t);
    fprintf(f, "TD\t%f\n", TD);
    fprintf(f, "tnt\t%f\n", params->tnt);
    fprintf(f, "w\t%f\n", params->w);
    fprintf(f, "wtnt\t%f\n", params->wtnt);
    fprintf(f, "Demand (q) %.15e\n", q);
    fprintf(f, "Stress Distribution Factor (J) %.15e\n", j);
    fprintf(f, "Tolerable Pressure %.15e\n", q_hat_tol);
    fprintf(f, "Probability of Breakage (Pb) %.15e\n", pb);
    fprintf(f, "Capacity (LR)%.15e\n", lr);
    fprintf(f, "Non-Factored Load (NFL)%.15e\n", nfl);
    fprintf(f, "Safety Requirement-1 %d\n", is_safe1);
    fprintf(f, "Safety Requirement-2 %d\n", is_safe2);
    if (is_safe1 && is_safe2)
        fprintf(f, "For the given input parameters, the glass is considered safe\n");
    else
        fprintf(f, "For the given input parameters, the glass is NOT considered safe\n");  
    fclose(f);
}