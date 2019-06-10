package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class InputFormat {
    
    public static void func_get_input(String filename, InputParameters inParams, double τ, double A_tol, double R_tol) throws Exception {
        Scanner infile;
        String line;
        ArrayList<String> lines = new ArrayList<String>(0);
        ArrayList<String> linetokens = new ArrayList<String>(0);
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
        inParams.rho_W = Double.parseDouble(infile.nextLine());
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
        infile.close();
    }
}

