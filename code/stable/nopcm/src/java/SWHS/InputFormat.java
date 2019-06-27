package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class InputFormat {
    
    public static Object[] get_input(String filename) throws Exception {
        double A_C;
        double C_W;
        double h_C;
        double T_init;
        double t_final;
        double L;
        double T_C;
        double t_step;
        double rho_W;
        double D;
        double A_tol;
        double R_tol;
        double T_W;
        double E_W;
        
        Scanner infile;
        String line;
        ArrayList<String> lines = new ArrayList<String>(0);
        ArrayList<String> linetokens = new ArrayList<String>(0);
        infile = new Scanner(new File(filename));
        infile.nextLine();
        inParams.A_C = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.C_W = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.h_C = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.T_init = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.t_final = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.L = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.T_C = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.t_step = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.rho_W = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.D = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.A_tol = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.R_tol = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.T_W = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.E_W = Double.parseDouble(infile.nextLine());
        infile.close();
        
        Object[] outputs = new Object[14];
        outputs[0] = A_C;
        outputs[1] = C_W;
        outputs[2] = h_C;
        outputs[3] = T_init;
        outputs[4] = t_final;
        outputs[5] = L;
        outputs[6] = T_C;
        outputs[7] = t_step;
        outputs[8] = rho_W;
        outputs[9] = D;
        outputs[10] = A_tol;
        outputs[11] = R_tol;
        outputs[12] = T_W;
        outputs[13] = E_W;
        return outputs;
    }
}

