using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class ReadTable {
    
    public static void func_read_table(string filename, List<double> z_vector, List<List<double>> x_matrix, List<List<double>> y_matrix) {
        StreamReader infile;
        string line;
        List<string> lines = new List<string>(0);
        List<string> linetokens = new List<string>(0);
        infile = new StreamReader(filename);
        line = (infile.ReadLine());
        linetokens = new List<string>(line.Split(','));
        for (int j = 0; (j < (int)((linetokens.Count / 2))); j += 1) {
            z_vector.Add(Double.Parse(linetokens[((j * 2) + 1)]));
        }
        while (!(infile.EndOfStream)) {
            lines.Add(infile.ReadLine());
        }
        for (int i = 0; (i < lines.Count); i += 1) {
            linetokens = new List<string>(lines[i].Split(','));
            List<double> x_matrix_temp = new List<double> {};
            List<double> y_matrix_temp = new List<double> {};
            for (int j = 0; (j < (int)((linetokens.Count / 2))); j += 1) {
                x_matrix_temp.Add(Double.Parse(linetokens[((j * 2) + 0)]));
                y_matrix_temp.Add(Double.Parse(linetokens[((j * 2) + 1)]));
            }
            x_matrix.Add(x_matrix_temp);
            y_matrix.Add(y_matrix_temp);
        }
        infile.Close();
    }
}

