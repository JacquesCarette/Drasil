/** \file InputConstraints.cs
    \brief Provides the function for checking the physical constraints and software constraints on the input
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputConstraints {
    
    /** \brief Verifies that input values satisfy the physical constraints and software constraints
        \param inParams structure holding the input values
    */
    public static void input_constraints(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function input_constraints called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        if (!(0.1 <= inParams.a && inParams.a <= 5.0)) {
            throw new Exception("InputError");
        }
        if (!(0.1 <= inParams.b && inParams.b <= 5.0)) {
            throw new Exception("InputError");
        }
        if (!(4.5 <= inParams.w && inParams.w <= 910.0)) {
            throw new Exception("InputError");
        }
        if (!(6.0 <= inParams.SD && inParams.SD <= 130.0)) {
            throw new Exception("InputError");
        }
        if (!(inParams.AR <= 5.0)) {
            throw new Exception("InputError");
        }
        
        if (!(inParams.a > 0)) {
            throw new Exception("InputError");
        }
        if (!(inParams.a >= inParams.b)) {
            throw new Exception("InputError");
        }
        if (!(0 < inParams.b && inParams.b <= inParams.a)) {
            throw new Exception("InputError");
        }
        if (!(inParams.w > 0)) {
            throw new Exception("InputError");
        }
        if (!(0 < inParams.P_btol && inParams.P_btol < 1)) {
            throw new Exception("InputError");
        }
        if (!(inParams.TNT > 0)) {
            throw new Exception("InputError");
        }
        if (!(inParams.SD > 0)) {
            throw new Exception("InputError");
        }
        if (!(inParams.AR >= 1)) {
            throw new Exception("InputError");
        }
    }
}

