/** \file InputConstraints.cs
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
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
        if (!(inParams.v_launch > 0)) {
            Console.Write("Warning: ");
            Console.Write("v_launch has value ");
            Console.Write(inParams.v_launch);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
        if (!(0 < inParams.theta && inParams.theta < 3.14159265 / 2)) {
            Console.Write("Warning: ");
            Console.Write("theta has value ");
            Console.Write(inParams.theta);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(0);
            Console.Write(" and ");
            Console.Write(3.14159265 / 2);
            Console.Write(" ((pi)/(2))");
            Console.WriteLine(".");
        }
        if (!(inParams.p_target > 0)) {
            Console.Write("Warning: ");
            Console.Write("p_target has value ");
            Console.Write(inParams.p_target);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
    }
}

