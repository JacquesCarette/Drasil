package Projectile;

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
        inParams.v_launch = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.theta = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.p_target = Double.parseDouble(infile.nextLine());
        infile.close();
    }
}

