/** \file Calculations.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
using System;
using System.IO;

public class Calculations {
    
    /** \brief Calculates minimum thickness (m)
        \param inParams structure holding the input values
        \return minimum thickness (m)
    */
    public static double func_h(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_h called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        return 1.0 / 1000.0 * (inParams.t == 2.5 ? 2.16 : inParams.t == 2.7 ? 2.59 : inParams.t == 3.0 ? 2.92 : inParams.t == 4.0 ? 3.78 : inParams.t == 5.0 ? 4.57 : inParams.t == 6.0 ? 5.56 : inParams.t == 8.0 ? 7.42 : inParams.t == 10.0 ? 9.02 : inParams.t == 12.0 ? 11.91 : inParams.t == 16.0 ? 15.09 : inParams.t == 19.0 ? 18.26 : 21.44);
    }
    
    /** \brief Calculates glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
        \param inParams structure holding the input values
        \return glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
    */
    public static int func_GTF(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_GTF called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        if (inParams.g == "AN") {
            return 1;
        }
        else if (inParams.g == "FT") {
            return 4;
        }
        else if (inParams.g == "HS") {
            return 2;
        }
        else {
            throw new Exception("Undefined case encountered in function func_GTF");
        }
    }
    
    /** \brief Calculates aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
        \param inParams structure holding the input values
        \return aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
    */
    public static double func_AR(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_AR called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        return inParams.a / inParams.b;
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
        \param h minimum thickness (m)
        \param GTF glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
        \return dimensionless load
    */
    public static double func_q_hat(InputParameters inParams, double q, double h, int GTF) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_q_hat called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  q = ");
        outfile.Write(q);
        outfile.WriteLine(", ");
        outfile.Write("  h = ");
        outfile.Write(h);
        outfile.WriteLine(", ");
        outfile.Write("  GTF = ");
        outfile.WriteLine(GTF);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return q * Math.Pow(inParams.a * inParams.b, 2.0) / (7.17e10 * Math.Pow(h, 4.0) * GTF);
    }
    
    /** \brief Calculates stress distribution factor (Function) based on Pbtol
        \param inParams structure holding the input values
        \param h minimum thickness (m)
        \return stress distribution factor (Function) based on Pbtol
    */
    public static double func_J_tol(InputParameters inParams, double h) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_J_tol called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  h = ");
        outfile.WriteLine(h);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Math.Log(Math.Log(1.0 / (1.0 - inParams.P_btol)) * (Math.Pow(inParams.a * inParams.b, 7.0 - 1.0) / (2.86e-53 * Math.Pow(7.17e10 * Math.Pow(h, 2.0), 7.0) * inParams.LDF)));
    }
    
    /** \brief Calculates stress distribution factor (Function)
        \param AR aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
        \param q_hat dimensionless load
        \return stress distribution factor (Function)
    */
    public static double func_J(double AR, double q_hat) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_J called with inputs: {");
        outfile.Write("  AR = ");
        outfile.Write(AR);
        outfile.WriteLine(", ");
        outfile.Write("  q_hat = ");
        outfile.WriteLine(q_hat);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Interpolation.interpZ("SDF.txt", AR, q_hat);
    }
    
    /** \brief Calculates tolerable load
        \param AR aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
        \param J_tol stress distribution factor (Function) based on Pbtol
        \return tolerable load
    */
    public static double func_q_hat_tol(double AR, double J_tol) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_q_hat_tol called with inputs: {");
        outfile.Write("  AR = ");
        outfile.Write(AR);
        outfile.WriteLine(", ");
        outfile.Write("  J_tol = ");
        outfile.WriteLine(J_tol);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Interpolation.interpY("SDF.txt", AR, J_tol);
    }
    
    /** \brief Calculates risk of failure
        \param inParams structure holding the input values
        \param h minimum thickness (m)
        \param J stress distribution factor (Function)
        \return risk of failure
    */
    public static double func_B(InputParameters inParams, double h, double J) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_B called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  h = ");
        outfile.Write(h);
        outfile.WriteLine(", ");
        outfile.Write("  J = ");
        outfile.WriteLine(J);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return 2.86e-53 / Math.Pow(inParams.a * inParams.b, 7.0 - 1.0) * Math.Pow(7.17e10 * Math.Pow(h, 2.0), 7.0) * inParams.LDF * Math.Exp(J);
    }
    
    /** \brief Calculates non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
        \param inParams structure holding the input values
        \param q_hat_tol tolerable load
        \param h minimum thickness (m)
        \return non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    */
    public static double func_NFL(InputParameters inParams, double q_hat_tol, double h) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_NFL called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  q_hat_tol = ");
        outfile.Write(q_hat_tol);
        outfile.WriteLine(", ");
        outfile.Write("  h = ");
        outfile.WriteLine(h);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return q_hat_tol * 7.17e10 * Math.Pow(h, 4.0) / Math.Pow(inParams.a * inParams.b, 2.0);
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
    
    /** \brief Calculates load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
        \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
        \param GTF glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
        \return load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
    */
    public static double func_LR(double NFL, int GTF) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_LR called with inputs: {");
        outfile.Write("  NFL = ");
        outfile.Write(NFL);
        outfile.WriteLine(", ");
        outfile.Write("  GTF = ");
        outfile.WriteLine(GTF);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return NFL * GTF * 1.0;
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
}
