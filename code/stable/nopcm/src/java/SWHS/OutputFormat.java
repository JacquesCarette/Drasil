package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class OutputFormat {
    
    public static void write_output(InputParameters inParams) throws Exception {
        PrintWriter outputfile;
        outputfile = new PrintWriter(new FileWriter(new File("output.txt"), false));
        outputfile.print("T_W = ");
        outputfile.println(inParams.T_W);
        outputfile.print("E_W = ");
        outputfile.println(inParams.E_W);
        outputfile.close();
    }
}

