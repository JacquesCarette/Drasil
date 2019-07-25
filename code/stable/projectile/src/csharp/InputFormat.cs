/** \file InputFormat.cs
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputFormat {
    
    /** \brief Reads input from a file with the given file name
        \param filename No description given
        \param inParams No description given
    */
    public static void get_input(string filename, InputParameters inParams) {
        StreamReader infile;
        infile = new StreamReader(filename);
        infile.ReadLine();
        inParams.v_launch = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.theta = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.p_target = Double.Parse(infile.ReadLine());
        infile.Close();
    }
}

