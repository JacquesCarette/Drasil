package GlassBR;

/** \file Calculations.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class Calculations {
    
    /** \brief Calculates stress distribution factor (Function) based on Pbtol
        \param inParams structure holding the input values
    */
    public static double func_J_tol(InputParameters inParams) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_J_tol called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        return Math.log(Math.log(1 / (1 - inParams.P_btol)) * (Math.pow(inParams.a * inParams.b, 7.0 - 1) / (2.86e-53 * Math.pow(7.17e10 * Math.pow(inParams.h, 2), 7.0) * inParams.LDF)));
    }
    
    /** \brief Calculates applied load (demand)
        \param inParams structure holding the input values
    */
    public static double func_q(InputParameters inParams) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_q called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        return Interpolation.func_interpY("TSD.txt", inParams.SD, inParams.w_TNT);
    }
    
    /** \brief Calculates dimensionless load
        \param inParams structure holding the input values
        \param q applied load (demand) (Pa)
    */
    public static double func_q_hat(InputParameters inParams, double q) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_q_hat called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  q = ");
        outfile.println(q);
        outfile.println("  }");
        outfile.close();
        
        return q * Math.pow(inParams.a * inParams.b, 2) / (7.17e10 * Math.pow(inParams.h, 4) * inParams.GTF);
    }
    
    /** \brief Calculates tolerable load
        \param inParams structure holding the input values
        \param J_tol stress distribution factor (Function) based on Pbtol
    */
    public static double func_q_hat_tol(InputParameters inParams, double J_tol) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_q_hat_tol called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  J_tol = ");
        outfile.println(J_tol);
        outfile.println("  }");
        outfile.close();
        
        return Interpolation.func_interpY("SDF.txt", inParams.AR, J_tol);
    }
    
    /** \brief Calculates stress distribution factor (Function)
        \param inParams structure holding the input values
        \param q_hat dimensionless load
    */
    public static double func_J(InputParameters inParams, double q_hat) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_J called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  q_hat = ");
        outfile.println(q_hat);
        outfile.println("  }");
        outfile.close();
        
        return Interpolation.func_interpZ("SDF.txt", inParams.AR, q_hat);
    }
    
    /** \brief Calculates non-factored load
        \param inParams structure holding the input values
        \param q_hat_tol tolerable load
    */
    public static double func_NFL(InputParameters inParams, double q_hat_tol) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_NFL called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  q_hat_tol = ");
        outfile.println(q_hat_tol);
        outfile.println("  }");
        outfile.close();
        
        return q_hat_tol * 7.17e10 * Math.pow(inParams.h, 4) / Math.pow(inParams.a * inParams.b, 2);
    }
    
    /** \brief Calculates risk of failure
        \param inParams structure holding the input values
        \param J stress distribution factor (Function)
    */
    public static double func_B(InputParameters inParams, double J) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_B called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  J = ");
        outfile.println(J);
        outfile.println("  }");
        outfile.close();
        
        return 2.86e-53 / Math.pow(inParams.a * inParams.b, 7.0 - 1) * Math.pow(7.17e10 * Math.pow(inParams.h, 2), 7.0) * inParams.LDF * Math.exp(J);
    }
    
    /** \brief Calculates load resistance
        \param inParams structure holding the input values
        \param NFL non-factored load (Pa)
    */
    public static double func_LR(InputParameters inParams, double NFL) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_LR called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  NFL = ");
        outfile.println(NFL);
        outfile.println("  }");
        outfile.close();
        
        return NFL * inParams.GTF * 1;
    }
    
    /** \brief Calculates variable that is assigned true when load resistance (capacity) is greater than load (demand)
        \param LR load resistance (Pa)
        \param q applied load (demand) (Pa)
    */
    public static Boolean func_is_safeLR(double LR, double q) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_is_safeLR called with inputs: {");
        outfile.print("  LR = ");
        outfile.print(LR);
        outfile.println(", ");
        outfile.print("  q = ");
        outfile.println(q);
        outfile.println("  }");
        outfile.close();
        
        return LR > q;
    }
    
    /** \brief Calculates probability of breakage
        \param B risk of failure
    */
    public static double func_P_b(double B) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_P_b called with inputs: {");
        outfile.print("  B = ");
        outfile.println(B);
        outfile.println("  }");
        outfile.close();
        
        return 1 - Math.exp(-B);
    }
    
    /** \brief Calculates variable that is assigned true when calculated probability is less than tolerable probability
        \param inParams structure holding the input values
        \param P_b probability of breakage
    */
    public static Boolean func_is_safePb(InputParameters inParams, double P_b) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_is_safePb called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  P_b = ");
        outfile.println(P_b);
        outfile.println("  }");
        outfile.close();
        
        return P_b < inParams.P_btol;
    }
}

