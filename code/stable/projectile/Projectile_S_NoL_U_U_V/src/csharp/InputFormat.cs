/** \file InputFormat.cs
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for reading inputs
*/
using System;
using System.IO;

public class InputFormat {
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param p_target target position: the distance from the launcher to the target (m)
    */
    public static void get_input(string filename, out double v_launch, out double theta, out double p_target) {
        StreamReader infile;
        infile = new StreamReader(filename);
        infile.ReadLine();
        v_launch = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        theta = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        p_target = Double.Parse(infile.ReadLine());
        infile.Close();
    }
}
