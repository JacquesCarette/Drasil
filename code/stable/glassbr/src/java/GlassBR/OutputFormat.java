package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class OutputFormat {
    
    public static void write_output(Boolean is_safePb, Boolean is_safeLR, double P_b) throws Exception {
        PrintWriter outputfile;
        outputfile = new PrintWriter(new FileWriter(new File("output.txt"), false));
        outputfile.print("is_safePb = ");
        outputfile.println(is_safePb);
        outputfile.print("is_safeLR = ");
        outputfile.println(is_safeLR);
        outputfile.print("P_b = ");
        outputfile.println(P_b);
        outputfile.close();
    }
}

