/* Input Format Module

This module takes a filename and then uses it to load the parameter
values into the parameters structure (params).

Authors: Thulasi Jegatheesan, Spencer Smith, Ned Nedialkov, and Brooks
MacLachlan

Date Last Revised: June 10, 2016

State Variables: none

Environment Variables: the file associated with the filename

Assumptions: Once initialized, the parameter values are constant for the
life of the program.

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "parameters.h"
#include "load_params.h"
#define pi 3.1415926535897932384626433832795

struct parameters load_params(const char inputFile[]){
    //Extract data from input parameters file
    FILE * fPointer;
    fPointer = fopen(inputFile, "r");
    double param[21];
    int counter = 0;
    char *endptr;
    char currentLine[25];
    while(1){
        fgets(currentLine, 25, fPointer);
        if(currentLine[0] != '#'){
            double thisLine = strtod(currentLine, &endptr);
            param[counter] = thisLine;
        }
        else continue;
        counter += 1;
        if(counter == 21){
            break;
        }
    }
    fclose(fPointer);

    struct parameters params;
    //parameters from input

    params.L       = param[0];
    params.diam    = param[1];
    params.Vp      = param[2];
    params.Ap      = param[3];
    params.rho_p   = param[4];
    params.Tmelt   = param[5];
    params.C_ps    = param[6];
    params.C_pl    = param[7];
    params.Hf      = param[8];
    params.Ac      = param[9];
    params.Tc      = param[10];
    params.rho_w   = param[11];
    params.C_w     = param[12];
    params.hc      = param[13];
    params.hp      = param[14];
    params.Tinit   = param[15];
    params.tstep   = param[16];
    params.tfinal  = param[17];
    params.AbsTol  = param[18];
    params.RelTol  = param[19];
    params.ConsTol = param[20];
    //calculated parameters

    params.Vt          = pi  * pow(params.diam / 2, 2) * params.L;
    params.Mw          = params.rho_w * (params.Vt - params.Vp);
    params.tau_w       = (params.Mw * params.C_w) / (params.hc * params.Ac);
    params.eta         = (params.hp * params.Ap) / (params.hc * params.Ac);
    params.Mp          = params.rho_p * params.Vp;
    params.tau_ps      = (params.Mp * params.C_ps) / (params.hp * params.Ap);
    params.tau_pl      = (params.Mp * params.C_pl) / (params.hp * params.Ap);
    params.Epmelt_init = params.C_ps * params.Mp * (params.Tmelt - params.Tinit);
    params.Ep_melt3    = params.Hf * params.Mp;
    params.Mw_noPCM    = params.rho_w * params.Vt;
    params.tau_w_noPCM = (params.Mw_noPCM * params.C_w) / (params.hc * params.Ac);


    return params;
}
