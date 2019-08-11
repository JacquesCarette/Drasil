/** \file ReadTable.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides a function for reading glass ASTM data
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class ReadTable {
    
    /** \brief Reads glass ASTM data from a file with the given file name
        \param filename name of file with x y and z data
        \param z_vector list of z values
        \param x_matrix lists of x values at different z values
        \param y_matrix lists of y values at different z values
    */
    public static void func_read_table(string filename, List<double> z_vector, List<List<double>> x_matrix, List<List<double>> y_matrix) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_read_table called with inputs: {");
        outfile.Write("  filename = ");
        outfile.Write(filename);
        outfile.WriteLine(", ");
        outfile.Write("  z_vector = ");
        outfile.Write("[");
        for (int list_i1 = 0; list_i1 < z_vector.Count - 1; list_i1++) {
            outfile.Write(z_vector[list_i1]);
            outfile.Write(", /f ");
        }
        if (z_vector.Count > 0) {
            outfile.Write(z_vector[z_vector.Count - 1]);
        }
        outfile.Write("]");
        outfile.WriteLine(", ");
        outfile.Write("  x_matrix = ");
        outfile.Write("[");
        for (int list_i2 = 0; list_i2 < x_matrix.Count - 1; list_i2++) {
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < x_matrix[list_i2].Count - 1; list_i1++) {
                outfile.Write(x_matrix[list_i2][list_i1]);
                outfile.Write(", /f ");
            }
            if (x_matrix[list_i2].Count > 0) {
                outfile.Write(x_matrix[list_i2][x_matrix[list_i2].Count - 1]);
            }
            outfile.Write("]");
            outfile.Write(", /f ");
        }
        if (x_matrix.Count > 0) {
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < x_matrix[x_matrix.Count - 1].Count - 1; list_i1++) {
                outfile.Write(x_matrix[x_matrix.Count - 1][list_i1]);
                outfile.Write(", /f ");
            }
            if (x_matrix[x_matrix.Count - 1].Count > 0) {
                outfile.Write(x_matrix[x_matrix.Count - 1][x_matrix[x_matrix.Count - 1].Count - 1]);
            }
            outfile.Write("]");
        }
        outfile.Write("]");
        outfile.WriteLine(", ");
        outfile.Write("  y_matrix = ");
        outfile.Write("[");
        for (int list_i2 = 0; list_i2 < y_matrix.Count - 1; list_i2++) {
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < y_matrix[list_i2].Count - 1; list_i1++) {
                outfile.Write(y_matrix[list_i2][list_i1]);
                outfile.Write(", /f ");
            }
            if (y_matrix[list_i2].Count > 0) {
                outfile.Write(y_matrix[list_i2][y_matrix[list_i2].Count - 1]);
            }
            outfile.Write("]");
            outfile.Write(", /f ");
        }
        if (y_matrix.Count > 0) {
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < y_matrix[y_matrix.Count - 1].Count - 1; list_i1++) {
                outfile.Write(y_matrix[y_matrix.Count - 1][list_i1]);
                outfile.Write(", /f ");
            }
            if (y_matrix[y_matrix.Count - 1].Count > 0) {
                outfile.Write(y_matrix[y_matrix.Count - 1][y_matrix[y_matrix.Count - 1].Count - 1]);
            }
            outfile.Write("]");
        }
        outfile.WriteLine("]");
        outfile.WriteLine("  }");
        outfile.Close();
        
        StreamReader infile;
        string line;
        List<string> linetokens = new List<string>(0);
        List<string> lines = new List<string>(0);
        infile = new StreamReader(filename);
        line = (infile.ReadLine());
        linetokens = new List<string>(line.Split(','));
        for (int stringlist_i = 0; stringlist_i < linetokens.Count / 1; stringlist_i += 1) {
            z_vector.Add(Double.Parse(linetokens[stringlist_i * 1 + 0]));
        }
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'z_vector' assigned to ");
        outfile.Write("[");
        for (int list_i1 = 0; list_i1 < z_vector.Count - 1; list_i1++) {
            outfile.Write(z_vector[list_i1]);
            outfile.Write(", /f ");
        }
        if (z_vector.Count > 0) {
            outfile.Write(z_vector[z_vector.Count - 1]);
        }
        outfile.Write("]");
        outfile.WriteLine(" in module ReadTable");
        outfile.Close();
        while (!(infile.EndOfStream)) {
            lines.Add(infile.ReadLine());
        }
        for (int i = 0; i < lines.Count; i += 1) {
            linetokens = new List<string>(lines[i].Split(','));
            List<double> x_matrix_temp = new List<double> {};
            List<double> y_matrix_temp = new List<double> {};
            for (int stringlist_i = 0; stringlist_i < linetokens.Count / 2; stringlist_i += 1) {
                x_matrix_temp.Add(Double.Parse(linetokens[stringlist_i * 2 + 0]));
                y_matrix_temp.Add(Double.Parse(linetokens[stringlist_i * 2 + 1]));
            }
            x_matrix.Add(x_matrix_temp);
            y_matrix.Add(y_matrix_temp);
        }
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'x_matrix' assigned to ");
        outfile.Write("[");
        for (int list_i2 = 0; list_i2 < x_matrix.Count - 1; list_i2++) {
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < x_matrix[list_i2].Count - 1; list_i1++) {
                outfile.Write(x_matrix[list_i2][list_i1]);
                outfile.Write(", /f ");
            }
            if (x_matrix[list_i2].Count > 0) {
                outfile.Write(x_matrix[list_i2][x_matrix[list_i2].Count - 1]);
            }
            outfile.Write("]");
            outfile.Write(", /f ");
        }
        if (x_matrix.Count > 0) {
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < x_matrix[x_matrix.Count - 1].Count - 1; list_i1++) {
                outfile.Write(x_matrix[x_matrix.Count - 1][list_i1]);
                outfile.Write(", /f ");
            }
            if (x_matrix[x_matrix.Count - 1].Count > 0) {
                outfile.Write(x_matrix[x_matrix.Count - 1][x_matrix[x_matrix.Count - 1].Count - 1]);
            }
            outfile.Write("]");
        }
        outfile.Write("]");
        outfile.WriteLine(" in module ReadTable");
        outfile.Close();
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'y_matrix' assigned to ");
        outfile.Write("[");
        for (int list_i2 = 0; list_i2 < y_matrix.Count - 1; list_i2++) {
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < y_matrix[list_i2].Count - 1; list_i1++) {
                outfile.Write(y_matrix[list_i2][list_i1]);
                outfile.Write(", /f ");
            }
            if (y_matrix[list_i2].Count > 0) {
                outfile.Write(y_matrix[list_i2][y_matrix[list_i2].Count - 1]);
            }
            outfile.Write("]");
            outfile.Write(", /f ");
        }
        if (y_matrix.Count > 0) {
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < y_matrix[y_matrix.Count - 1].Count - 1; list_i1++) {
                outfile.Write(y_matrix[y_matrix.Count - 1][list_i1]);
                outfile.Write(", /f ");
            }
            if (y_matrix[y_matrix.Count - 1].Count > 0) {
                outfile.Write(y_matrix[y_matrix.Count - 1][y_matrix[y_matrix.Count - 1].Count - 1]);
            }
            outfile.Write("]");
        }
        outfile.Write("]");
        outfile.WriteLine(" in module ReadTable");
        outfile.Close();
        infile.Close();
    }
}

