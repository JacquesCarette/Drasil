using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputFormat {
    
    public static void get_input(string filename, ref double A_C, ref double C_W, ref double h_C, ref double T_init, ref double t_final, ref double L, ref double T_C, ref double t_step, ref double rho_W, ref double D, ref double A_tol, ref double R_tol, ref double T_W, ref double E_W) {
        StreamReader infile;
        string line;
        List<string> lines = new List<string>(0);
        List<string> linetokens = new List<string>(0);
        infile = new StreamReader(filename);
        infile.ReadLine();
        inParams.A_C = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.C_W = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.h_C = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.T_init = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.t_final = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.L = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.T_C = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.t_step = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.rho_W = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.D = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.A_tol = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.R_tol = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.T_W = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.E_W = Double.Parse(infile.ReadLine());
        infile.Close();
    }
}

