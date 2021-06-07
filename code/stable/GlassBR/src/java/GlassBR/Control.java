package GlassBR;

/** \file Control.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Controls the flow of the program
*/
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void main(String[] args) throws Exception, FileNotFoundException, IOException {
        PrintWriter outfile;
        String filename = args[0];
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'filename' assigned ");
        outfile.print(filename);
        outfile.println(" in module Control");
        outfile.close();
        InputParameters inParams = new InputParameters();
        InputFormat.get_input(filename, inParams);
        DerivedValues.derived_values(inParams);
        InputConstraints.input_constraints(inParams);
        double J_tol = Calculations.func_J_tol(inParams);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'J_tol' assigned ");
        outfile.print(J_tol);
        outfile.println(" in module Control");
        outfile.close();
        double q = Calculations.func_q(inParams);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'q' assigned ");
        outfile.print(q);
        outfile.println(" in module Control");
        outfile.close();
        double q_hat = Calculations.func_q_hat(inParams, q);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'q_hat' assigned ");
        outfile.print(q_hat);
        outfile.println(" in module Control");
        outfile.close();
        double q_hat_tol = Calculations.func_q_hat_tol(inParams, J_tol);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'q_hat_tol' assigned ");
        outfile.print(q_hat_tol);
        outfile.println(" in module Control");
        outfile.close();
        double J = Calculations.func_J(inParams, q_hat);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'J' assigned ");
        outfile.print(J);
        outfile.println(" in module Control");
        outfile.close();
        double NFL = Calculations.func_NFL(inParams, q_hat_tol);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'NFL' assigned ");
        outfile.print(NFL);
        outfile.println(" in module Control");
        outfile.close();
        double B = Calculations.func_B(inParams, J);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'B' assigned ");
        outfile.print(B);
        outfile.println(" in module Control");
        outfile.close();
        double LR = Calculations.func_LR(inParams, NFL);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'LR' assigned ");
        outfile.print(LR);
        outfile.println(" in module Control");
        outfile.close();
        boolean is_safeLR = Calculations.func_is_safeLR(LR, q);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'is_safeLR' assigned ");
        outfile.print(is_safeLR);
        outfile.println(" in module Control");
        outfile.close();
        double P_b = Calculations.func_P_b(B);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'P_b' assigned ");
        outfile.print(P_b);
        outfile.println(" in module Control");
        outfile.close();
        boolean is_safePb = Calculations.func_is_safePb(inParams, P_b);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'is_safePb' assigned ");
        outfile.print(is_safePb);
        outfile.println(" in module Control");
        outfile.close();
        OutputFormat.write_output(is_safePb, is_safeLR, P_b, J);
    }
}
