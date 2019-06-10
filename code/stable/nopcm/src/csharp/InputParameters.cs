using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputParameters {
    public static double A_C = 0.0;
    public static double C_W = 0.0;
    public static double h_C = 0.0;
    public static double T_init = 0.0;
    public static double t_final = 0.0;
    public static double L = 0.0;
    public static double T_C = 0.0;
    public static double rho_W = 0.0;
    public static double D = 0.0;
    public static double T_W = 0.0;
    public static double E_W = 0.0;
    
    public static void derived_values() {
    }
    
    public static void input_constraints(InputParameters inParams) {
        if (!((inParams.A_C <= A_C_max))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((C_W_min < inParams.C_W) && (inParams.C_W < C_W_max)))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((h_C_min <= inParams.h_C) && (inParams.h_C <= h_C_max)))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!((inParams.t_final < t_final_max))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((L_min <= inParams.L) && (inParams.L <= L_max)))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((rho_W_min < inParams.rho_W) && (inParams.rho_W <= rho_W_max)))) {
            Console.WriteLine("Warning: constraint violated");
        }
        
        if (!((inParams.A_C > 0))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!((inParams.C_W > 0))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!((inParams.h_C > 0))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((0 < inParams.T_init) && (inParams.T_init < 100)))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!((inParams.t_final > 0))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!((inParams.L > 0))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((0 < inParams.T_C) && (inParams.T_C < 100)))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!((inParams.rho_W > 0))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!((inParams.D > 0))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((inParams.T_init <= inParams.T_W) && (inParams.T_W <= inParams.T_C)))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!((inParams.E_W >= 0))) {
            Console.WriteLine("Warning: constraint violated");
        }
    }
}

