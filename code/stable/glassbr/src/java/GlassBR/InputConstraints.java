package GlassBR;

/** \file InputConstraints.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for checking the physical constraints and software constraints on the input
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class InputConstraints {
    
    /** \brief Verifies that input values satisfy the physical constraints and software constraints
        \param inParams structure holding the input values
    */
    public static void input_constraints(InputParameters inParams) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function input_constraints called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        if (!(0.1 <= inParams.a && inParams.a <= 5.0)) {
            System.out.print("a has value ");
            System.out.print(inParams.a);
            System.out.print(" but expected to be ");
            System.out.print("between ");
            System.out.print(0.1);
            System.out.print(" (d_min)");
            System.out.print(" and ");
            System.out.print(5.0);
            System.out.print(" (d_max)");
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(0.1 <= inParams.b && inParams.b <= 5.0)) {
            System.out.print("b has value ");
            System.out.print(inParams.b);
            System.out.print(" but expected to be ");
            System.out.print("between ");
            System.out.print(0.1);
            System.out.print(" (d_min)");
            System.out.print(" and ");
            System.out.print(5.0);
            System.out.print(" (d_max)");
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(4.5 <= inParams.w && inParams.w <= 910.0)) {
            System.out.print("w has value ");
            System.out.print(inParams.w);
            System.out.print(" but expected to be ");
            System.out.print("between ");
            System.out.print(4.5);
            System.out.print(" (w_min)");
            System.out.print(" and ");
            System.out.print(910.0);
            System.out.print(" (w_max)");
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(6.0 <= inParams.SD && inParams.SD <= 130.0)) {
            System.out.print("SD has value ");
            System.out.print(inParams.SD);
            System.out.print(" but expected to be ");
            System.out.print("between ");
            System.out.print(6.0);
            System.out.print(" (SD_min)");
            System.out.print(" and ");
            System.out.print(130.0);
            System.out.print(" (SD_max)");
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(inParams.AR <= 5.0)) {
            System.out.print("AR has value ");
            System.out.print(inParams.AR);
            System.out.print(" but expected to be ");
            System.out.print("below ");
            System.out.print(5.0);
            System.out.print(" (AR_max)");
            System.out.println(".");
            throw new Exception("InputError");
        }
        
        if (!(inParams.a > 0)) {
            System.out.print("a has value ");
            System.out.print(inParams.a);
            System.out.print(" but expected to be ");
            System.out.print("above ");
            System.out.print(0);
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(inParams.a >= inParams.b)) {
            System.out.print("a has value ");
            System.out.print(inParams.a);
            System.out.print(" but expected to be ");
            System.out.print("above ");
            System.out.print(inParams.b);
            System.out.print(" (b)");
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(0 < inParams.b && inParams.b <= inParams.a)) {
            System.out.print("b has value ");
            System.out.print(inParams.b);
            System.out.print(" but expected to be ");
            System.out.print("between ");
            System.out.print(0);
            System.out.print(" and ");
            System.out.print(inParams.a);
            System.out.print(" (a)");
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(inParams.w > 0)) {
            System.out.print("w has value ");
            System.out.print(inParams.w);
            System.out.print(" but expected to be ");
            System.out.print("above ");
            System.out.print(0);
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(0 <= inParams.P_btol && inParams.P_btol <= 1)) {
            System.out.print("P_btol has value ");
            System.out.print(inParams.P_btol);
            System.out.print(" but expected to be ");
            System.out.print("between ");
            System.out.print(0);
            System.out.print(" and ");
            System.out.print(1);
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(inParams.TNT > 0)) {
            System.out.print("TNT has value ");
            System.out.print(inParams.TNT);
            System.out.print(" but expected to be ");
            System.out.print("above ");
            System.out.print(0);
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(inParams.SD > 0)) {
            System.out.print("SD has value ");
            System.out.print(inParams.SD);
            System.out.print(" but expected to be ");
            System.out.print("above ");
            System.out.print(0);
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(inParams.AR >= 1)) {
            System.out.print("AR has value ");
            System.out.print(inParams.AR);
            System.out.print(" but expected to be ");
            System.out.print("above ");
            System.out.print(1);
            System.out.println(".");
            throw new Exception("InputError");
        }
    }
}

