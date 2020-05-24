package GlassBR;

/** \file OutputFormat.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param is_safePb probability of glass breakage safety requirement
        \param is_safeLR 3 second load equivalent resistance safety requirement
        \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
        \param J stress distribution factor (Function)
    */
    public static void write_output(boolean is_safePb, boolean is_safeLR, double P_b, double J) throws IOException {
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
        outfile.print(P_b);
        outfile.println(", ");
        outfile.print("  J = ");
        outfile.println(J);
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
        outputfile.print("J = ");
        outputfile.println(J);
        outputfile.close();
    }
}
