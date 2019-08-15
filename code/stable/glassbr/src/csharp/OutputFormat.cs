/** \file OutputFormat.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param is_safePb probability of glass breakage safety requirement
        \param is_safeLR 3 second load equivalent resistance safety requirement
        \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
    */
    public static void write_output(Boolean is_safePb, Boolean is_safeLR, double P_b) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function write_output called with inputs: {");
        outfile.Write("  is_safePb = ");
        outfile.Write(is_safePb);
        outfile.WriteLine(", ");
        outfile.Write("  is_safeLR = ");
        outfile.Write(is_safeLR);
        outfile.WriteLine(", ");
        outfile.Write("  P_b = ");
        outfile.WriteLine(P_b);
        outfile.WriteLine("  }");
        outfile.Close();
        
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("is_safePb = ");
        outputfile.WriteLine(is_safePb);
        outputfile.Write("is_safeLR = ");
        outputfile.WriteLine(is_safeLR);
        outputfile.Write("P_b = ");
        outputfile.WriteLine(P_b);
        outputfile.Close();
    }
}

