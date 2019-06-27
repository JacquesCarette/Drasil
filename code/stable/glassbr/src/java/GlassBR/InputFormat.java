package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class InputFormat {
    
    public static void get_input(String filename, InputParameters inParams) throws Exception {
        Scanner infile;
        String line;
        ArrayList<String> lines = new ArrayList<String>(0);
        ArrayList<String> linetokens = new ArrayList<String>(0);
        infile = new Scanner(new File(filename));
        infile.nextLine();
        inParams.a = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.b = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.w = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.P_btol = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.TNT = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.g = (infile.nextLine());
        infile.nextLine();
        inParams.t = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.SD_x = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.SD_y = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.SD_z = Double.parseDouble(infile.nextLine());
        infile.close();
    }
}

