package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class InputConstraints {
    
    public static void input_constraints(InputParameters inParams) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function input_constraints called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        if (!(((0.1 <= inParams.a) && (inParams.a <= 5.0)))) {
            throw new Exception("InputError");
        }
        if (!(((0.1 <= inParams.b) && (inParams.b <= 5.0)))) {
            throw new Exception("InputError");
        }
        if (!(((4.5 <= inParams.w) && (inParams.w <= 910.0)))) {
            throw new Exception("InputError");
        }
        if (!(((6.0 <= inParams.SD) && (inParams.SD <= 130.0)))) {
            throw new Exception("InputError");
        }
        if (!((inParams.AR <= 5.0))) {
            throw new Exception("InputError");
        }
        
        if (!((inParams.a > 0))) {
            throw new Exception("InputError");
        }
        if (!((inParams.a >= inParams.b))) {
            throw new Exception("InputError");
        }
        if (!(((0 < inParams.b) && (inParams.b <= inParams.a)))) {
            throw new Exception("InputError");
        }
        if (!((inParams.w > 0))) {
            throw new Exception("InputError");
        }
        if (!(((0 < inParams.P_btol) && (inParams.P_btol < 1)))) {
            throw new Exception("InputError");
        }
        if (!((inParams.TNT > 0))) {
            throw new Exception("InputError");
        }
        if (!((inParams.SD > 0))) {
            throw new Exception("InputError");
        }
        if (!((inParams.AR >= 1))) {
            throw new Exception("InputError");
        }
    }
}

