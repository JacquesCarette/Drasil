package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class InputFormat {
    
    public static void func_get_inputs(String filename, InputParameters inParams, double τ, double A_tol, double R_tol, double C_tol) throws Exception {
        Scanner infile;
        String line;
        Vector<String> lines = new Vector<String>(0);
        Vector<String> linetokens = new Vector<String>(0);
        infile = new Scanner(new File(filename));
        infile.nextLine();
        inParams.L = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.D = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.A_C = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.T_C = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.ρ_W = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.C_W = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.h_C = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.T_init = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        τ = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.t_final = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        A_tol = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        R_tol = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        C_tol = Double.parseDouble(infile.nextLine());
        infile.close();
    }
}

