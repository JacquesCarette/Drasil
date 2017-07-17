package GlassBR_program;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class OutputFormat {
    
    public static void write_output(String filename, Boolean is_safe1, Boolean is_safe2, double P_b) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(filename);
        outfile.print("is_safe1 = ");
        outfile.println(is_safe1);
        outfile.print("is_safe2 = ");
        outfile.println(is_safe2);
        outfile.print("P_b = ");
        outfile.println(P_b);
        outfile.close();
    }
}

