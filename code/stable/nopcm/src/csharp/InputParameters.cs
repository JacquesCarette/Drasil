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
        if (!((inParams.A_C <= 100000))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((4170 < inParams.C_W) && (inParams.C_W < 4210)))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((10 <= inParams.h_C) && (inParams.h_C <= 10000)))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!((inParams.t_final < 86400))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((0.1 <= inParams.L) && (inParams.L <= 50)))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((950 < inParams.rho_W) && (inParams.rho_W <= 1000)))) {
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

