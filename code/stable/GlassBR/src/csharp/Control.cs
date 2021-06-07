/** \file Control.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Controls the flow of the program
*/
using System;
using System.IO;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void Main(string[] args) {
        StreamWriter outfile;
        string filename = args[0];
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'filename' assigned ");
        outfile.Write(filename);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        InputParameters inParams = new InputParameters();
        InputFormat.get_input(filename, inParams);
        DerivedValues.derived_values(inParams);
        InputConstraints.input_constraints(inParams);
        double J_tol = Calculations.func_J_tol(inParams);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'J_tol' assigned ");
        outfile.Write(J_tol);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        double q = Calculations.func_q(inParams);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'q' assigned ");
        outfile.Write(q);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        double q_hat = Calculations.func_q_hat(inParams, q);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'q_hat' assigned ");
        outfile.Write(q_hat);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        double q_hat_tol = Calculations.func_q_hat_tol(inParams, J_tol);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'q_hat_tol' assigned ");
        outfile.Write(q_hat_tol);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        double J = Calculations.func_J(inParams, q_hat);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'J' assigned ");
        outfile.Write(J);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        double NFL = Calculations.func_NFL(inParams, q_hat_tol);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'NFL' assigned ");
        outfile.Write(NFL);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        double B = Calculations.func_B(inParams, J);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'B' assigned ");
        outfile.Write(B);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        double LR = Calculations.func_LR(inParams, NFL);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'LR' assigned ");
        outfile.Write(LR);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        Boolean is_safeLR = Calculations.func_is_safeLR(LR, q);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'is_safeLR' assigned ");
        outfile.Write(is_safeLR);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        double P_b = Calculations.func_P_b(B);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'P_b' assigned ");
        outfile.Write(P_b);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        Boolean is_safePb = Calculations.func_is_safePb(inParams, P_b);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'is_safePb' assigned ");
        outfile.Write(is_safePb);
        outfile.WriteLine(" in module Control");
        outfile.Close();
        OutputFormat.write_output(is_safePb, is_safeLR, P_b, J);
    }
}
