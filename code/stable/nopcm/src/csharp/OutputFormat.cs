/** \file OutputFormat.cs
    \author Thulasi Jegatheesan
    \brief Provides the function for writing outputs
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
        \param E_W change in heat energy in the water: change in thermal energy within the water (J)
    */
    public static void write_output(double T_W, double E_W) {
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("T_W = ");
        outputfile.WriteLine(T_W);
        outputfile.Write("E_W = ");
        outputfile.WriteLine(E_W);
        outputfile.Close();
    }
}

