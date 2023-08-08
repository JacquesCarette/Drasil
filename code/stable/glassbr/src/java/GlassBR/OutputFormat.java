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
    public static void write_output(InputParameters inParams, double B, double J, double NFL, double q_hat, double q_hat_tol, double J_tol, double P_b, double LR, boolean isSafePb, boolean isSafeLR) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function write_output called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  B = ");
        outfile.print(B);
        outfile.println(", ");
        outfile.print("  J = ");
        outfile.print(J);
        outfile.println(", ");
        outfile.print("  NFL = ");
        outfile.print(NFL);
        outfile.println(", ");
        outfile.print("  q_hat = ");
        outfile.print(q_hat);
        outfile.println(", ");
        outfile.print("  q_hat_tol = ");
        outfile.print(q_hat_tol);
        outfile.println(", ");
        outfile.print("  J_tol = ");
        outfile.print(J_tol);
        outfile.println(", ");
        outfile.print("  P_b = ");
        outfile.print(P_b);
        outfile.println(", ");
        outfile.print("  LR = ");
        outfile.print(LR);
        outfile.println(", ");
        outfile.print("  isSafePb = ");
        outfile.print(isSafePb);
        outfile.println(", ");
        outfile.print("  isSafeLR = ");
        outfile.println(isSafeLR);
        outfile.println("  }");
        outfile.close();
        
        PrintWriter outputfile;
        outputfile = new PrintWriter(new FileWriter(new File("output.txt"), false));
        outputfile.print("B = ");
        outputfile.println(B);
        outputfile.print("J = ");
        outputfile.println(J);
        outputfile.print("NFL = ");
        outputfile.println(NFL);
        outputfile.print("q_hat = ");
        outputfile.println(q_hat);
        outputfile.print("q_hat_tol = ");
        outputfile.println(q_hat_tol);
        outputfile.print("J_tol = ");
        outputfile.println(J_tol);
        outputfile.print("P_b = ");
        outputfile.println(P_b);
        outputfile.print("LR = ");
        outputfile.println(LR);
        outputfile.print("isSafePb = ");
        outputfile.println(isSafePb);
        outputfile.print("isSafeLR = ");
        outputfile.println(isSafeLR);
        outputfile.print("GTF = ");
        outputfile.println(inParams.GTF);
        outputfile.print("h = ");
        outputfile.println(inParams.h);
        outputfile.print("AR = ");
        outputfile.println(inParams.AR);
        outputfile.close();
    }
}
