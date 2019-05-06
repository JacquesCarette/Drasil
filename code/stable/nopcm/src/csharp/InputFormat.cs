using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace SWHS {
    public class InputFormat {
        
        public static void func_get_inputs(string filename, InputParameters inParams, double τ, double A_tol, double R_tol) {
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
            inParams.ρ_W = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            inParams.C_W = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            inParams.h_C = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            inParams.T_init = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            τ = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            inParams.t_final = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            A_tol = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            R_tol = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            C_tol = Double.Parse(infile.ReadLine());
            infile.Close();
        }
    }
}

