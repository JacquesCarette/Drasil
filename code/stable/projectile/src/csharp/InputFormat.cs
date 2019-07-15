using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputFormat {
    
    public static void get_input(string filename, InputParameters inParams) {
        StreamReader infile;
        infile = new StreamReader(filename);
        infile.ReadLine();
        inParams.v_launch = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.angle = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.p_target = Double.Parse(infile.ReadLine());
        infile.Close();
    }
}

