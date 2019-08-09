package GlassBR;

/** \file OutputFormat.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param is_safePb variable that is assigned true when calculated probability is less than tolerable probability
        \param is_safeLR variable that is assigned true when load resistance (capacity) is greater than load (demand)
        \param P_b probability of breakage
    */
    public static void write_output(Boolean is_safePb, Boolean is_safeLR, double P_b) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function write_output called with inputs: {");
        outfile.print("  is_safePb = ");
        outfile.print(is_safePb);
        outfile.println(", ");
        outfile.print("  is_safeLR = ");
        outfile.print(is_safeLR);
        outfile.println(", ");
        outfile.print("  P_b = ");
        outfile.println(P_b);
        outfile.println("  }");
        outfile.close();
        
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

