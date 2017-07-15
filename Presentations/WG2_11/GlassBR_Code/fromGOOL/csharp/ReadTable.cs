using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class ReadTable {
        
        public static List<double> read_z_array(string filename) {
            StreamReader infile;
            infile = new StreamReader(filename);
            string line;
            line = infile.ReadLine();
            infile.Close();
            List<string> z_array_str = new List<string>(0);
            z_array_str = new List<string>(line.Split(','));
            {   List<string> temp = new List<string>(0);
                for (int i_temp = 1; i_temp < z_array_str.Count; i_temp += 2) {
                    temp.Add(z_array_str[i_temp]);
                }
                z_array_str = temp;
            }
            List<double> z_array = new List<double>(0);
            for (int i = 0; i < z_array_str.Count; i++) {
                z_array.Add(Double.Parse(z_array_str[i]));
            }
            return z_array;
        }
        
        public static List< List<double> > read_x_array(string filename) {
            StreamReader infile;
            infile = new StreamReader(filename);
            List<string> lines = new List<string>(0);
            while (!(infile).EndOfStream) {
                lines.Add(infile.ReadLine());
            }
            infile.Close();
            {   List<string> temp = new List<string>(0);
                for (int i_temp = 1; i_temp < lines.Count; i_temp++) {
                    temp.Add(lines[i_temp]);
                }
                lines = temp;
            }
            List< List<string> > x_array_str = new List< List<string> >(0);
            for (int i = 0; i < lines.Count; i++) {
                List<string> temp_str = new List<string>(0);
                temp_str = new List<string>(lines[i].Split(','));
                {   List<string> temp = new List<string>(0);
                    for (int i_temp = 0; i_temp < temp_str.Count; i_temp += 2) {
                        temp.Add(temp_str[i_temp]);
                    }
                    temp_str = temp;
                }
                x_array_str.Add(temp_str);
            }
            List< List<double> > x_array = new List< List<double> >(0);
            for (int i = 0; i < x_array_str.Count; i++) {
                List<double> nextLine = new List<double>(0);
                for (int j = 0; j < x_array_str[i].Count; j++) {
                    nextLine.Add(Double.Parse(x_array_str[i][j]));
                }
                x_array.Add(nextLine);
            }
            return x_array;
        }
        
        public static List< List<double> > read_y_array(string filename) {
            StreamReader infile;
            infile = new StreamReader(filename);
            List<string> lines = new List<string>(0);
            while (!(infile).EndOfStream) {
                lines.Add(infile.ReadLine());
            }
            infile.Close();
            {   List<string> temp = new List<string>(0);
                for (int i_temp = 1; i_temp < lines.Count; i_temp++) {
                    temp.Add(lines[i_temp]);
                }
                lines = temp;
            }
            List< List<string> > y_array_str = new List< List<string> >(0);
            for (int i = 0; i < lines.Count; i++) {
                List<string> temp_str = new List<string>(0);
                temp_str = new List<string>(lines[i].Split(','));
                {   List<string> temp = new List<string>(0);
                    for (int i_temp = 1; i_temp < temp_str.Count; i_temp += 2) {
                        temp.Add(temp_str[i_temp]);
                    }
                    temp_str = temp;
                }
                y_array_str.Add(temp_str);
            }
            List< List<double> > y_array = new List< List<double> >(0);
            for (int i = 0; i < y_array_str.Count; i++) {
                List<double> nextLine = new List<double>(0);
                for (int j = 0; j < y_array_str[i].Count; j++) {
                    nextLine.Add(Double.Parse(y_array_str[i][j]));
                }
                y_array.Add(nextLine);
            }
            return y_array;
        }
    }
}

