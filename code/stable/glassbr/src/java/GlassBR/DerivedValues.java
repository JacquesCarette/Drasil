package GlassBR;

/** \file DerivedValues.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for calculating derived values
*/
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class DerivedValues {
    
    /** \brief Calculates values that can be immediately derived from the inputs
        \param inParams structure holding the input values
    */
    public static void derived_values(InputParameters inParams) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function derived_values called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        inParams.LDF = Math.pow(3.0 / 60.0, 7.0 / 16.0);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.LDF' assigned ");
        outfile.print(inParams.LDF);
        outfile.println(" in module DerivedValues");
        outfile.close();
        
        inParams.SD = Math.sqrt(Math.pow(inParams.SD_x, 2.0) + Math.pow(inParams.SD_y, 2.0) + Math.pow(inParams.SD_z, 2.0));
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.SD' assigned ");
        outfile.print(inParams.SD);
        outfile.println(" in module DerivedValues");
        outfile.close();
        
        inParams.w_TNT = inParams.w * inParams.TNT;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.w_TNT' assigned ");
        outfile.print(inParams.w_TNT);
        outfile.println(" in module DerivedValues");
        outfile.close();
    }
}
