/** \file Calculations.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
using System;
using System.IO;

public class Calculations {
    
    /** \brief Calculates stress distribution factor (Function) based on Pbtol
        \param inParams structure holding the input values
        \return stress distribution factor (Function) based on Pbtol
    */
    public static double func_J_tol(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_J_tol called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Math.Log(Math.Log(1.0 / (1.0 - inParams.P_btol)) * (Math.Pow(inParams.a * inParams.b, 7.0 - 1.0) / (2.86e-53 * Math.Pow(7.17e10 * Math.Pow(inParams.h, 2.0), 7.0) * inParams.LDF)));
    }
    
    /** \brief Calculates applied load (demand): 3 second duration equivalent pressure (Pa)
        \param inParams structure holding the input values
        \return applied load (demand): 3 second duration equivalent pressure (Pa)
    */
    public static double func_q(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_q called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Interpolation.interpY("TSD.txt", inParams.SD, inParams.w_TNT);
    }
    
    /** \brief Calculates dimensionless load
        \param inParams structure holding the input values
        \param q applied load (demand): 3 second duration equivalent pressure (Pa)
        \return dimensionless load
    */
    public static double func_q_hat(InputParameters inParams, double q) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_q_hat called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  q = ");
        outfile.WriteLine(q);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return q * Math.Pow(inParams.a * inParams.b, 2.0) / (7.17e10 * Math.Pow(inParams.h, 4.0) * inParams.GTF);
    }
    
    /** \brief Calculates tolerable load
        \param inParams structure holding the input values
        \param J_tol stress distribution factor (Function) based on Pbtol
        \return tolerable load
    */
    public static double func_q_hat_tol(InputParameters inParams, double J_tol) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_q_hat_tol called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  J_tol = ");
        outfile.WriteLine(J_tol);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Interpolation.interpY("SDF.txt", inParams.AR, J_tol);
    }
    
    /** \brief Calculates stress distribution factor (Function)
        \param inParams structure holding the input values
        \param q_hat dimensionless load
        \return stress distribution factor (Function)
    */
    public static double func_J(InputParameters inParams, double q_hat) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_J called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  q_hat = ");
        outfile.WriteLine(q_hat);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Interpolation.interpZ("SDF.txt", inParams.AR, q_hat);
    }
    
    /** \brief Calculates non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
        \param inParams structure holding the input values
        \param q_hat_tol tolerable load
        \return non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    */
    public static double func_NFL(InputParameters inParams, double q_hat_tol) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_NFL called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  q_hat_tol = ");
        outfile.WriteLine(q_hat_tol);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return q_hat_tol * 7.17e10 * Math.Pow(inParams.h, 4.0) / Math.Pow(inParams.a * inParams.b, 2.0);
    }
    
    /** \brief Calculates risk of failure
        \param inParams structure holding the input values
        \param J stress distribution factor (Function)
        \return risk of failure
    */
    public static double func_B(InputParameters inParams, double J) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_B called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  J = ");
        outfile.WriteLine(J);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return 2.86e-53 / Math.Pow(inParams.a * inParams.b, 7.0 - 1.0) * Math.Pow(7.17e10 * Math.Pow(inParams.h, 2.0), 7.0) * inParams.LDF * Math.Exp(J);
    }
    
    /** \brief Calculates load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
        \param inParams structure holding the input values
        \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
        \return load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
    */
    public static double func_LR(InputParameters inParams, double NFL) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_LR called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  NFL = ");
        outfile.WriteLine(NFL);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return NFL * inParams.GTF * 1.0;
    }
    
    /** \brief Calculates 3 second load equivalent resistance safety requirement
        \param LR load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
        \param q applied load (demand): 3 second duration equivalent pressure (Pa)
        \return 3 second load equivalent resistance safety requirement
    */
    public static Boolean func_isSafeLR(double LR, double q) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_isSafeLR called with inputs: {");
        outfile.Write("  LR = ");
        outfile.Write(LR);
        outfile.WriteLine(", ");
        outfile.Write("  q = ");
        outfile.WriteLine(q);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return LR > q;
    }
    
    /** \brief Calculates probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
        \param B risk of failure
        \return probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
    */
    public static double func_P_b(double B) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_P_b called with inputs: {");
        outfile.Write("  B = ");
        outfile.WriteLine(B);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return 1.0 - Math.Exp(-B);
    }
    
    /** \brief Calculates probability of glass breakage safety requirement
        \param inParams structure holding the input values
        \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
        \return probability of glass breakage safety requirement
    */
    public static Boolean func_isSafePb(InputParameters inParams, double P_b) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_isSafePb called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  P_b = ");
        outfile.WriteLine(P_b);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return P_b < inParams.P_btol;
    }
}