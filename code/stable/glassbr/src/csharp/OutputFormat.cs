/** \file OutputFormat.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
using System;
using System.IO;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param inParams structure holding the input values
        \param isSafePb Safety Req-Pb
        \param isSafeLR Safety Req-LR
        \param B risk of failure
        \param J stress distribution factor (Function)
        \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
        \param q_hat dimensionless load
        \param q_hat_tol tolerable load
        \param J_tol stress distribution factor (Function) based on Pbtol
    */
    public static void write_output(InputParameters inParams, Boolean isSafePb, Boolean isSafeLR, double B, double J, double NFL, double q_hat, double q_hat_tol, double J_tol) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function write_output called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  isSafePb = ");
        outfile.Write(isSafePb);
        outfile.WriteLine(", ");
        outfile.Write("  isSafeLR = ");
        outfile.Write(isSafeLR);
        outfile.WriteLine(", ");
        outfile.Write("  B = ");
        outfile.Write(B);
        outfile.WriteLine(", ");
        outfile.Write("  J = ");
        outfile.Write(J);
        outfile.WriteLine(", ");
        outfile.Write("  NFL = ");
        outfile.Write(NFL);
        outfile.WriteLine(", ");
        outfile.Write("  q_hat = ");
        outfile.Write(q_hat);
        outfile.WriteLine(", ");
        outfile.Write("  q_hat_tol = ");
        outfile.Write(q_hat_tol);
        outfile.WriteLine(", ");
        outfile.Write("  J_tol = ");
        outfile.WriteLine(J_tol);
        outfile.WriteLine("  }");
        outfile.Close();
        
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("isSafePb = ");
        outputfile.WriteLine(isSafePb);
        outputfile.Write("isSafeLR = ");
        outputfile.WriteLine(isSafeLR);
        outputfile.Write("B = ");
        outputfile.WriteLine(B);
        outputfile.Write("J = ");
        outputfile.WriteLine(J);
        outputfile.Write("NFL = ");
        outputfile.WriteLine(NFL);
        outputfile.Write("GTF = ");
        outputfile.WriteLine(inParams.GTF);
        outputfile.Write("q_hat = ");
        outputfile.WriteLine(q_hat);
        outputfile.Write("q_hat_tol = ");
        outputfile.WriteLine(q_hat_tol);
        outputfile.Write("J_tol = ");
        outputfile.WriteLine(J_tol);
        outputfile.Write("h = ");
        outputfile.WriteLine(inParams.h);
        outputfile.Write("AR = ");
        outputfile.WriteLine(inParams.AR);
        outputfile.Close();
    }
}
