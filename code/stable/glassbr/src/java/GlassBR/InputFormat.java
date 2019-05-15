package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class InputFormat {
    
    public static void func_get_input(String filename, InputParameters inParams) throws Exception {
        Scanner infile;
        String line;
        Vector<String> lines = new Vector<String>(0);
        Vector<String> linetokens = new Vector<String>(0);
        infile = new Scanner(new File(filename));
        infile.nextLine();
        inParams.a = Double.parseDouble(infile.nextLine());
        inParams.b = Double.parseDouble(infile.nextLine());
        inParams.t = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.g =(infile.nextLine());
        infile.nextLine();
        inParams.w = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.TNT = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.SD_x = Double.parseDouble(infile.nextLine());
        inParams.SD_y = Double.parseDouble(infile.nextLine());
        inParams.SD_z = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.P_btol = Double.parseDouble(infile.nextLine());
        infile.close();
    }
}

