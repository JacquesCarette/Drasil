using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class Control {
    
    public static void Main(string[] args) {
        string filename = args[0];
        double inParams.A_C;
        double inParams.C_W;
        double inParams.h_C;
        double inParams.T_init;
        double inParams.t_final;
        double inParams.L;
        double inParams.T_C;
        double inParams.t_step;
        double inParams.rho_W;
        double inParams.D;
        double inParams.A_tol;
        double inParams.R_tol;
        double inParams.T_W;
        double inParams.E_W;
        InputFormat.get_input(filename, ref A_C, ref C_W, ref h_C, ref T_init, ref t_final, ref L, ref T_C, ref t_step, ref rho_W, ref D, ref A_tol, ref R_tol, ref T_W, ref E_W);
        InputParameters.input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W);
        OutputFormat.write_output(T_W, E_W);
    }
}

