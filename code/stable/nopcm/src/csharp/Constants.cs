/** \file Constants.cs
    \author Thulasi Jegatheesan
    \brief Provides the structure for holding constant values
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

/** \brief Structure for holding the constant values
*/
public class Constants {
    public double L_min;
    public double L_max;
    public double rho_W_min;
    public double rho_W_max;
    public double A_C_max;
    public double C_W_min;
    public double C_W_max;
    public double h_C_min;
    public double h_C_max;
    public double t_final_max;
    public double AR_min;
    public double AR_max;
    
    /** \brief Assigns values to variables for constants
    */
    public Constants() {
        this.L_min = 0.1;
        this.L_max = 50;
        this.rho_W_min = 950;
        this.rho_W_max = 1000;
        this.A_C_max = 100000;
        this.C_W_min = 4170;
        this.C_W_max = 4210;
        this.h_C_min = 10;
        this.h_C_max = 10000;
        this.t_final_max = 86400;
        this.AR_min = 1.0e-2;
        this.AR_max = 100;
    }
}

