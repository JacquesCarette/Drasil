/** \file InputConstraints.cs
    \author Nikitha Krithnan and W. Spencer Smith
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
            Console.Write("a has value ");
            Console.Write(inParams.a);
            Console.Write(" but expected to be ");
            Console.Write("between ");
            Console.Write(0.1);
            Console.Write(" (d_min)");
            Console.Write(" and ");
            Console.Write(5.0);
            Console.Write(" (d_max)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(0.1 <= inParams.b && inParams.b <= 5.0)) {
            Console.Write("b has value ");
            Console.Write(inParams.b);
            Console.Write(" but expected to be ");
            Console.Write("between ");
            Console.Write(0.1);
            Console.Write(" (d_min)");
            Console.Write(" and ");
            Console.Write(5.0);
            Console.Write(" (d_max)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(4.5 <= inParams.w && inParams.w <= 910.0)) {
            Console.Write("w has value ");
            Console.Write(inParams.w);
            Console.Write(" but expected to be ");
            Console.Write("between ");
            Console.Write(4.5);
            Console.Write(" (w_min)");
            Console.Write(" and ");
            Console.Write(910.0);
            Console.Write(" (w_max)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(6.0 <= inParams.SD && inParams.SD <= 130.0)) {
            Console.Write("SD has value ");
            Console.Write(inParams.SD);
            Console.Write(" but expected to be ");
            Console.Write("between ");
            Console.Write(6.0);
            Console.Write(" (SD_min)");
            Console.Write(" and ");
            Console.Write(130.0);
            Console.Write(" (SD_max)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(inParams.AR <= 5.0)) {
            Console.Write("AR has value ");
            Console.Write(inParams.AR);
            Console.Write(" but expected to be ");
            Console.Write("below ");
            Console.Write(5.0);
            Console.Write(" (AR_max)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        
        if (!(inParams.a > 0)) {
            Console.Write("a has value ");
            Console.Write(inParams.a);
            Console.Write(" but expected to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(inParams.a >= inParams.b)) {
            Console.Write("a has value ");
            Console.Write(inParams.a);
            Console.Write(" but expected to be ");
            Console.Write("above ");
            Console.Write(inParams.b);
            Console.Write(" (b)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(0 < inParams.b && inParams.b <= inParams.a)) {
            Console.Write("b has value ");
            Console.Write(inParams.b);
            Console.Write(" but expected to be ");
            Console.Write("between ");
            Console.Write(0);
            Console.Write(" and ");
            Console.Write(inParams.a);
            Console.Write(" (a)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(inParams.w > 0)) {
            Console.Write("w has value ");
            Console.Write(inParams.w);
            Console.Write(" but expected to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(0 < inParams.P_btol && inParams.P_btol < 1)) {
            Console.Write("P_btol has value ");
            Console.Write(inParams.P_btol);
            Console.Write(" but expected to be ");
            Console.Write("between ");
            Console.Write(0);
            Console.Write(" and ");
            Console.Write(1);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(inParams.TNT > 0)) {
            Console.Write("TNT has value ");
            Console.Write(inParams.TNT);
            Console.Write(" but expected to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(inParams.SD > 0)) {
            Console.Write("SD has value ");
            Console.Write(inParams.SD);
            Console.Write(" but expected to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(inParams.AR >= 1)) {
            Console.Write("AR has value ");
            Console.Write(inParams.AR);
            Console.Write(" but expected to be ");
            Console.Write("above ");
            Console.Write(1);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
    }
}

