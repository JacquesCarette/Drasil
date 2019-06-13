using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputFormat {
    
    public static void func_get_input(string filename, double L, double D, double A_C, double T_C, double rho_W, double C_W, double h_C, double T_init, double t_step, double t_final, double A_tol, double R_tol) {
        StreamReader infile;
        string line;
        List<string> lines = new List<string>(0);
        List<string> linetokens = new List<string>(0);
        infile = new StreamReader(filename);
        infile.ReadLine();
        inParams.L = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.D = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.A_C = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.T_C = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.rho_W = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.C_W = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.h_C = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.T_init = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.t_step = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.t_final = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.A_tol = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.R_tol = Double.Parse(infile.ReadLine());
        infile.Close();
    }
}

