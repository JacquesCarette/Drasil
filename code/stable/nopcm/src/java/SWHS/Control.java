package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class Control {
    
    public static void main(String[] args) throws Exception {
        String inputfile = args[0];
        InputParameters inParams = new InputParameters();
        func_get_input(inputfile, inParams);
        DerivedValues.derived_values(inParams);
        InputConstraints.input_constraints(inParams);
        OutputFormat.write_output(inParams);
    }
}

