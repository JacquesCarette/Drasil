/** \file DerivedValues.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for calculating derived values
*/
using System;
using System.IO;

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
        
        inParams.LDF = Math.Pow(3.0 / 60.0, 7.0 / 16.0);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.LDF' assigned ");
        outfile.Write(inParams.LDF);
        outfile.WriteLine(" in module DerivedValues");
        outfile.Close();
        
        inParams.SD = Math.Sqrt(Math.Pow(inParams.SD_x, 2.0) + Math.Pow(inParams.SD_y, 2.0) + Math.Pow(inParams.SD_z, 2.0));
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.SD' assigned ");
        outfile.Write(inParams.SD);
        outfile.WriteLine(" in module DerivedValues");
        outfile.Close();
        
        inParams.w_TNT = inParams.w * inParams.TNT;
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.w_TNT' assigned ");
        outfile.Write(inParams.w_TNT);
        outfile.WriteLine(" in module DerivedValues");
        outfile.Close();
    }
}
