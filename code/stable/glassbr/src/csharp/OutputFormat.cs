/** \file OutputFormat.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
using System;
using System.IO;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param inParams structure holding the input values
        \param B risk of failure
        \param J stress distribution factor (Function)
        \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
        \param q_hat dimensionless load
        \param q_hat_tol tolerable load
        \param J_tol stress distribution factor (Function) based on Pbtol
        \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
        \param LR load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
        \param isSafePb Safety Req-Pb
        \param isSafeLR Safety Req-LR
    */
    public static void write_output(InputParameters inParams, double B, double J, double NFL, double q_hat, double q_hat_tol, double J_tol, double P_b, double LR, Boolean isSafePb, Boolean isSafeLR) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function write_output called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
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
        outfile.Write(J_tol);
        outfile.WriteLine(", ");
        outfile.Write("  P_b = ");
        outfile.Write(P_b);
        outfile.WriteLine(", ");
        outfile.Write("  LR = ");
        outfile.Write(LR);
        outfile.WriteLine(", ");
        outfile.Write("  isSafePb = ");
        outfile.Write(isSafePb);
        outfile.WriteLine(", ");
        outfile.Write("  isSafeLR = ");
        outfile.WriteLine(isSafeLR);
        outfile.WriteLine("  }");
        outfile.Close();
        
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("B = ");
        outputfile.WriteLine(B);
        outputfile.Write("J = ");
        outputfile.WriteLine(J);
        outputfile.Write("NFL = ");
        outputfile.WriteLine(NFL);
        outputfile.Write("q_hat = ");
        outputfile.WriteLine(q_hat);
        outputfile.Write("q_hat_tol = ");
        outputfile.WriteLine(q_hat_tol);
        outputfile.Write("J_tol = ");
        outputfile.WriteLine(J_tol);
        outputfile.Write("P_b = ");
        outputfile.WriteLine(P_b);
        outputfile.Write("LR = ");
        outputfile.WriteLine(LR);
        outputfile.Write("isSafePb = ");
        outputfile.WriteLine(isSafePb);
        outputfile.Write("isSafeLR = ");
        outputfile.WriteLine(isSafeLR);
        outputfile.Write("GTF = ");
        outputfile.WriteLine(inParams.GTF);
        outputfile.Write("h = ");
        outputfile.WriteLine(inParams.h);
        outputfile.Write("AR = ");
        outputfile.WriteLine(inParams.AR);
        outputfile.Close();
    }
}
