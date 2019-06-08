using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class Control {
    
    public static void Main(string[] args) {
        string inputfile = args[0];
        InputParameters inParams = new InputParameters();
        InputFormat.func_get_input(inputfile, inParams);
        DerivedValues.derived_values(inParams);
        InputConstraints.input_constraints(inParams);
        OutputFormat.write_output(inParams);
    }
}

