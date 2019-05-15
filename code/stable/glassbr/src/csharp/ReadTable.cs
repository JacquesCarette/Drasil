using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class ReadTable {
        
        public static void func_read_table(string filename, List<double> z_vector, List< List<double> > x_matrix, List< List<double> > y_matrix) {
            StreamReader infile;
            string line;
            List<string> lines = new List<string>(0);
            List<string> linetokens = new List<string>(0);
            infile = new StreamReader(filename);
            line = infile.ReadLine();
            linetokens = new List<string>(line.Split(','));
            for (int j = 0; j < (int)(linetokens.Count / 2); j++) {
                while (z_vector.Count <= j) {
                    z_vector.Add(0.0);
                }
                z_vector[j] = Double.Parse(linetokens[(j * 2) + 1]);
            }
            while (!(infile).EndOfStream) {
                lines.Add(infile.ReadLine());
            }
            for (int i = 0; i < lines.Count; i++) {
                linetokens = new List<string>(lines[i].Split(','));
                for (int j = 0; j < (int)(linetokens.Count / 2); j++) {
                    while (x_matrix.Count <= i) {
                        x_matrix.Add(new List<double>());
                    }
                    while (x_matrix[i].Count <= j) {
                        x_matrix[i].Add(0.0);
                    }
                    x_matrix[i][j] = Double.Parse(linetokens[(j * 2) + 0]);
                    while (y_matrix.Count <= i) {
                        y_matrix.Add(new List<double>());
                    }
                    while (y_matrix[i].Count <= j) {
                        y_matrix[i].Add(0.0);
                    }
                    y_matrix[i][j] = Double.Parse(linetokens[(j * 2) + 1]);
                }
            }
            infile.Close();
        }
    }
}

