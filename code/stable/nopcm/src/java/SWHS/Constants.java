package SWHS;

/** \file Constants.java
    \author Thulasi Jegatheesan
    \brief Provides the structure for holding constant values
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

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
    public Constants() throws Exception {
        L_min = 0.1;
        L_max = 50;
        rho_W_min = 950;
        rho_W_max = 1000;
        A_C_max = 100000;
        C_W_min = 4170;
        C_W_max = 4210;
        h_C_min = 10;
        h_C_max = 10000;
        t_final_max = 86400;
        AR_min = 1.0e-2;
        AR_max = 100;
    }
}

