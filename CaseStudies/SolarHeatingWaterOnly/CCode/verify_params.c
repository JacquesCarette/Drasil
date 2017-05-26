/* Input Verification Module

This module verifies that the input parameters comply with physical and
software constraints by throwing errors and warnings, respectively, if
any parameter does not.

Authors: Thulasi Jegatheesan, Spencer Smith, Ned Nedialkov, and Brooks
MacLachlan

Date Last Revised: June 10, 2016

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "parameters.h"
#include "load_params.h"
#include "verify_params.h"
#define pi 3.1415926535897932384626433832795

int verify_valid(struct parameters params){

    int err = 0;

    // Check that inputs are valid

    if(params.L <= 0){
        printf("Error: Tank length must be > 0\n");
        err = 1;
    }
    else if(params.diam <= 0){
        printf("Error: Tank diameter must be > 0\n");
        err = 2;
    }
    else if(params.Vp <= 0){
        printf("Error: PCM volume must be > 0\n");
        err = 3;
    }
    else if(params.Vp >= params.Vt){
        printf("Error: PCM volume must be < tank volume\n");
        err = 4;
    }
    else if(params.Ap <= 0){
        printf("Error: PCM area must be > 0\n");
        err = 5;
    }
    else if(params.rho_p <= 0){
        printf("Error: rho_p must be > 0\n");
        err = 6;
    }
    else if(params.Tmelt <= 0 || params.Tmelt >= params.Tc){
        printf("Error: Tmelt must be > 0 and < Tc\n");
        err = 7;
    }
    else if(params.Tc <= params.Tinit){
        printf("Error: Tc must be > Tinit\n");
        err = 8;
    }
    else if(params.Tc >= 100 || params.Tc <= 0){
        printf("Error: Tc must be > 0 and < 100\n");
        err = 9;
    }
    else if(params.C_ps <= 0){
        printf("Error: C_ps must be > 0\n");
        err = 10;
    }
    else if(params.C_pl <= 0){
        printf("Error: C_pl must be > 0\n");
        err = 11;
    }
    else if(params.Hf <= 0){
        printf("Error: Hf must be > 0\n");
        err = 12;
    }
    else if(params.Ac <= 0){
        printf("Error: Ac must be > 0\n");
        err = 13;
    }
    else if(params.rho_w <= 0){
        printf("Error: rho_w must be > 0\n");
        err = 14;
    }
    else if(params.C_w <= 0){
        printf("Error: C_w must be > 0\n");
        err = 15;
    }
    else if(params.hc <= 0){
        printf("Error: hc must be > 0\n");
        err = 16;
    }
    else if(params.hp <= 0){
        printf("Error: hp must be > 0\n");
        err = 17;
    }
    else if(params.Tinit <= 0 || params.Tinit >= 100){
        printf("Error: Tinit must be > 0 and < 100\n");
        err = 18;
    }
    else if(params.tfinal <= 0){
        printf("Error: tfinal must be > 0\n");
        err = 19;
    }
    else if(params.Tinit >= params.Tmelt){
        printf("Error: Tinit must be < Tmelt\n");
        err = 20;
    }
    return err;
}

const char * verify_recommended(struct parameters params){

    const char * msg;

    // Software Constraints

    if(params.L < 0.1 || params.L > 50){
        msg = "Warning: It is recommended that 0.1 <= L <= 50";
        printf("%s\n", msg);
    }
    if(params.diam / params.L < 0.002 || params.diam / params.L > 200){
        msg = "Warning: It is recommended that 0.002 <= D/L <= 200";
        printf("%s\n", msg);
    }
    if(params.Vp < pow(10, -6) * params.Vt){
        msg = "Warning: It is recommended that Vp be >= 0.0001% of Vt";
        printf("%s\n", msg);
    }
    if(params.Vp > params.Ap || params.Ap > (2/0.001) * params.Vp){
        msg = "Warning: It is recommended that Vp <= Ap <= (2/0.001) * Vp";
        printf("%s\n", msg);
    }
    if(params.rho_p <= 500 || params.rho_p >= 20000){
        msg = "Warning: It is recommended that 500 < rho_p < 20000";
        printf("%s\n", msg);
    }
    if(params.C_ps <= 100 || params.C_ps >= 4000){
        msg = "Warning: It is recommended that 100 < C_ps < 4000";
        printf("%s\n", msg);
    }
    if(params.C_pl <= 100 || params.C_pl >= 5000){
        msg = "Warning: It is recommended that 100 < C_pl < 5000";
        printf("%s\n", msg);
    }
    /*if(params.Hf <= ADD WHEN DECIDED){
        warning
    }*/
    if(params.Ac > pi * pow(params.diam / 2, 2)){
        msg = "Warning: It is recommended that Ac <= pi * (D/2) ^ 2";
        printf("%s\n", msg);
    }
    if(params.rho_w <= 950 || params.rho_w > 1000){
        msg = "Warning: It is recommended that 950 < rho_w <= 1000";
        printf("%s\n", msg);
    }
    if(params.C_w <= 4170 || params.C_w >= 4210){
        msg = "Warning: It is recommended that 4170 < C_w < 4210";
        printf("%s\n", msg);
    }
    if(params.hc <= 10 || params.hc >= 10000){
        msg = "Warning: It is recommended that 10 < hc < 10000";
        printf("%s\n", msg);
    }
    if(params.hp <= 10 || params.hp >= 10000){
        msg = "Warning: It is recommended that 10 < hp < 10000";
        printf("%s\n", msg);
    }
    if(params.tfinal <= 0 || params.tfinal >= 86400){
        msg = "Warning: It is recommended that 0 < tfinal < 86400";
        printf("%s\n", msg);
    }

    return msg;
}
