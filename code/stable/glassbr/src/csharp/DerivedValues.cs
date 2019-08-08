/** \file DerivedValues.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for calculating derived values
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class DerivedValues {
    
    /** \brief Calculates values that can be immediately derived from the inputs
        \param inParams structure holding the input values
    */
    public static void derived_values(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function derived_values called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        inParams.h = 1.0 / 1000.0 * (inParams.t == 2.5 ? 2.16 : inParams.t == 2.7 ? 2.59 : inParams.t == 3.0 ? 2.92 : inParams.t == 4.0 ? 3.78 : inParams.t == 5.0 ? 4.57 : inParams.t == 6.0 ? 5.56 : inParams.t == 8.0 ? 7.42 : inParams.t == 10.0 ? 9.02 : inParams.t == 12.0 ? 11.91 : inParams.t == 16.0 ? 15.09 : inParams.t == 19.0 ? 18.26 : 21.44);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.h' assigned to ");
        outfile.Write(inParams.h);
        outfile.WriteLine(" in module DerivedValues");
        outfile.Close();
        
        inParams.LDF = Math.Pow(3.0 / 60, 7.0 / 16);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.LDF' assigned to ");
        outfile.Write(inParams.LDF);
        outfile.WriteLine(" in module DerivedValues");
        outfile.Close();
        
        if (inParams.g == "AN") {
            inParams.GTF = 1;
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'inParams.GTF' assigned to ");
            outfile.Write(inParams.GTF);
            outfile.WriteLine(" in module DerivedValues");
            outfile.Close();
        }
        else if (inParams.g == "FT") {
            inParams.GTF = 4;
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'inParams.GTF' assigned to ");
            outfile.Write(inParams.GTF);
            outfile.WriteLine(" in module DerivedValues");
            outfile.Close();
        }
        else if (inParams.g == "HS") {
            inParams.GTF = 2;
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'inParams.GTF' assigned to ");
            outfile.Write(inParams.GTF);
            outfile.WriteLine(" in module DerivedValues");
            outfile.Close();
        }
        else {
            throw new Exception("Undefined case encountered in function GTF");
        }
        
        inParams.SD = Math.Sqrt(Math.Pow(inParams.SD_x, 2) + Math.Pow(inParams.SD_y, 2) + Math.Pow(inParams.SD_z, 2));
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.SD' assigned to ");
        outfile.Write(inParams.SD);
        outfile.WriteLine(" in module DerivedValues");
        outfile.Close();
        
        inParams.AR = inParams.a / inParams.b;
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.AR' assigned to ");
        outfile.Write(inParams.AR);
        outfile.WriteLine(" in module DerivedValues");
        outfile.Close();
        
        inParams.w_TNT = inParams.w * inParams.TNT;
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.w_TNT' assigned to ");
        outfile.Write(inParams.w_TNT);
        outfile.WriteLine(" in module DerivedValues");
        outfile.Close();
    }
}

