/** \file InputFormat.cs
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for reading inputs
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputFormat {
    
    /** \brief Reads input from a file with the given file name
        \param inParams structure holding the input values
        \param filename name of the input file
    */
    public static void get_input(InputParameters inParams, string filename) {
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
