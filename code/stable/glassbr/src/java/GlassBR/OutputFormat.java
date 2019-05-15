package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class OutputFormat {
    
    public static void write_output(Boolean is_safePb, Boolean is_safeLR, double P_b) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter("output.txt");
        outfile.print("is_safePb = ");
        outfile.println(is_safePb);
        outfile.print("is_safeLR = ");
        outfile.println(is_safeLR);
        outfile.print("P_b = ");
        outfile.println(P_b);
        outfile.close();
    }
}

