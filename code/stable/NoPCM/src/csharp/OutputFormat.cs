/** \file OutputFormat.cs
    \author Thulasi Jegatheesan
    \brief Provides the function for writing outputs
*/
using System;
using System.Collections.Generic;
using System.IO;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param E_W change in heat energy in the water: change in thermal energy within the water (J)
        \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
    */
    public static void write_output(double E_W, List<double> T_W) {
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("T_W = ");
        outputfile.Write("[");
        for (int list_i1 = 0; list_i1 < T_W.Count - 1; list_i1++) {
            outputfile.Write(T_W[list_i1]);
            outputfile.Write(", ");
        }
        if (T_W.Count > 0) {
            outputfile.Write(T_W[T_W.Count - 1]);
        }
        outputfile.WriteLine("]");
        outputfile.Write("E_W = ");
        outputfile.WriteLine(E_W);
        outputfile.Close();
    }
}
