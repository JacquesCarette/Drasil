using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputConstraints {
    
    public static void input_constraints(InputParameters inParams) {
        if (!((inParams.v_launch > 0))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(((0 < inParams.angle) && (inParams.angle < (3.14159265 / 2))))) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!((inParams.p_target > 0))) {
            Console.WriteLine("Warning: constraint violated");
        }
    }
}

