/* Output Format Module

This module takes the parameter and output vales produced and
writes them to a file.

Authors: Thulasi Jegatheesan, Spencer Smith, Ned Nedialov, and Brooks
MacLachlan

Date Last Revised: June 24, 2016

State Variables: none

Environment Variables: the file associated with the filename

*/

#include <stdio.h>
#include <stdlib.h>
#include "parameters.h"
#include "output.h"

void output(char* filename, double time[], double tempWat[], double tempPCM[], double eWat[], double ePCM[], double eTot[], struct parameters params, int sizeOfResults){

    FILE * fPointer;
    fPointer = fopen(filename, "w");
    fprintf(fPointer, "\tL\t\t%f\n", params.L);
    fprintf(fPointer, "\tdiam\t\t%f\n", params.diam);
    fprintf(fPointer, "\tVp\t\t%f\n", params.Vp);
    fprintf(fPointer, "\tAp\t\t%f\n", params.Ap);
    fprintf(fPointer, "\trho_p\t\t%f\n", params.rho_p);
    fprintf(fPointer, "\tTmelt\t\t%f\n", params.Tmelt);
    fprintf(fPointer, "\tC_ps\t\t%f\n", params.C_ps);
    fprintf(fPointer, "\tC_pl\t\t%f\n", params.C_pl);
    fprintf(fPointer, "\tHf\t\t%f\n", params.Hf);
    fprintf(fPointer, "\tAc\t\t%f\n", params.Ac);
    fprintf(fPointer, "\tTc\t\t%f\n", params.Tc);
    fprintf(fPointer, "\trho_w\t\t%f\n", params.rho_w);
    fprintf(fPointer, "\tC_w\t\t%f\n", params.C_w);
    fprintf(fPointer, "\thc\t\t%f\n", params.hc);
    fprintf(fPointer, "\thp\t\t%f\n", params.hp);
    fprintf(fPointer, "\tTinit\t\t%f\n", params.Tinit);
    fprintf(fPointer, "\ttstep\t\t%f\n", params.tstep);
    fprintf(fPointer, "\ttfinal\t\t%f\n", params.tfinal);
    fprintf(fPointer, "\tAbsTol\t\t%f\n", params.AbsTol);
    fprintf(fPointer, "\tRelTol\t\t%f\n", params.RelTol);
    fprintf(fPointer, "\tConsTol\t\t%f\n", params.ConsTol);
    fprintf(fPointer, "\tVt\t\t%f\n", params.Vt);
    fprintf(fPointer, "\tMw\t\t%f\n", params.Mw);
    fprintf(fPointer, "\ttau_w\t\t%f\n", params.tau_w);
    fprintf(fPointer, "\teta\t\t%f\n", params.eta);
    fprintf(fPointer, "\tMp\t\t%f\n", params.Mp);
    fprintf(fPointer, "\ttau_ps\t\t%f\n", params.tau_ps);
    fprintf(fPointer, "\ttau_pl\t\t%f\n", params.tau_pl);
    fprintf(fPointer, "\tEpmelt_init\t%f\n", params.Epmelt_init);
    fprintf(fPointer, "\tEp_melt3\t%f\n", params.Ep_melt3);
    fprintf(fPointer, "\tMw_noPCM\t%f\n", params.Mw_noPCM);
    fprintf(fPointer, "\ttau_w_noPCM\t%f\n", params.tau_w_noPCM);
    fprintf(fPointer, "\nTime\t\tTwater\t\tTPCM\t\tEwater\t\tEPCM\t\tEtotal\n");
    int i;
    for(i = 0; i < sizeOfResults; i++){
        fprintf(fPointer,"%f\t%f\t%f\t%f\t%f\t%f\n", time[i], tempWat[i], tempPCM[i], eWat[i], ePCM[i], eTot[i]);
    }
    fclose(fPointer);

    return;
}
