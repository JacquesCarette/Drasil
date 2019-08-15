package GlassBR;

/** \file DerivedValues.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for calculating derived values
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class DerivedValues {
    
    /** \brief Calculates values that can be immediately derived from the inputs
        \param inParams structure holding the input values
    */
    public static void derived_values(InputParameters inParams) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function derived_values called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        inParams.h = 1.0 / 1000.0 * (inParams.t == 2.5 ? 2.16 : inParams.t == 2.7 ? 2.59 : inParams.t == 3.0 ? 2.92 : inParams.t == 4.0 ? 3.78 : inParams.t == 5.0 ? 4.57 : inParams.t == 6.0 ? 5.56 : inParams.t == 8.0 ? 7.42 : inParams.t == 10.0 ? 9.02 : inParams.t == 12.0 ? 11.91 : inParams.t == 16.0 ? 15.09 : inParams.t == 19.0 ? 18.26 : 21.44);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.h' assigned to ");
        outfile.print(inParams.h);
        outfile.println(" in module DerivedValues");
        outfile.close();
        
        inParams.LDF = Math.pow(3.0 / 60, 7.0 / 16);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.LDF' assigned to ");
        outfile.print(inParams.LDF);
        outfile.println(" in module DerivedValues");
        outfile.close();
        
        if (inParams.g.equals("AN")) {
            inParams.GTF = 1;
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'inParams.GTF' assigned to ");
            outfile.print(inParams.GTF);
            outfile.println(" in module DerivedValues");
            outfile.close();
        }
        else if (inParams.g.equals("FT")) {
            inParams.GTF = 4;
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'inParams.GTF' assigned to ");
            outfile.print(inParams.GTF);
            outfile.println(" in module DerivedValues");
            outfile.close();
        }
        else if (inParams.g.equals("HS")) {
            inParams.GTF = 2;
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'inParams.GTF' assigned to ");
            outfile.print(inParams.GTF);
            outfile.println(" in module DerivedValues");
            outfile.close();
        }
        else {
            throw new Exception("Undefined case encountered in function GTF");
        }
        
        inParams.SD = Math.sqrt(Math.pow(inParams.SD_x, 2) + Math.pow(inParams.SD_y, 2) + Math.pow(inParams.SD_z, 2));
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.SD' assigned to ");
        outfile.print(inParams.SD);
        outfile.println(" in module DerivedValues");
        outfile.close();
        
        inParams.AR = inParams.a / inParams.b;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.AR' assigned to ");
        outfile.print(inParams.AR);
        outfile.println(" in module DerivedValues");
        outfile.close();
        
        inParams.w_TNT = inParams.w * inParams.TNT;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.w_TNT' assigned to ");
        outfile.print(inParams.w_TNT);
        outfile.println(" in module DerivedValues");
        outfile.close();
    }
}

