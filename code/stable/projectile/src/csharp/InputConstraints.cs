/** \file InputConstraints.cs
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputConstraints {
    
    /** \brief Verifies that input values satisfy the physical constraints and software constraints
        \param inParams No description given
    */
    public static void input_constraints(InputParameters inParams) {
        if (!(inParams.v_launch > 0)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(0 < inParams.theta && inParams.theta < 3.14159265 / 2)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(inParams.p_target > 0)) {
            Console.WriteLine("Warning: constraint violated");
        }
    }
}

