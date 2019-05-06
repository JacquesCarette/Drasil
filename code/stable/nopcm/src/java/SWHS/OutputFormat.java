package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class OutputFormat {
    
    public static void write_output(InputParameters inParams) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter("output.txt");
        outfile.print("T_W = ");
        outfile.println(inParams.T_W);
        outfile.print("E_W = ");
        outfile.println(inParams.E_W);
        outfile.close();
    }
}

