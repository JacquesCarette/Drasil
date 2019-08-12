/** \file Interpolation.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides functions for linear interpolation on three-dimensional data
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class Interpolation {
    
    /** \brief Performs linear interpolation
        \param x_1 lower x-coordinate
        \param y_1 lower y-coordinate
        \param x_2 upper x-coordinate
        \param y_2 upper y-coordinate
        \param x x-coordinate to interpolate at
        \return y value interpolated at given x value
    */
    public static double func_lin_interp(double x_1, double y_1, double x_2, double y_2, double x) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_lin_interp called with inputs: {");
        outfile.Write("  x_1 = ");
        outfile.Write(x_1);
        outfile.WriteLine(", ");
        outfile.Write("  y_1 = ");
        outfile.Write(y_1);
        outfile.WriteLine(", ");
        outfile.Write("  x_2 = ");
        outfile.Write(x_2);
        outfile.WriteLine(", ");
        outfile.Write("  y_2 = ");
        outfile.Write(y_2);
        outfile.WriteLine(", ");
        outfile.Write("  x = ");
        outfile.WriteLine(x);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return (y_2 - y_1) / (x_2 - x_1) * (x - x_1) + y_1;
    }
    
    /** \brief Finds the array index for a value closest to the given value
        \param arr array in which value should be found
        \param v value whose index will be found
        \return index of given value in given array
    */
    public static int func_find(List<double> arr, double v) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_find called with inputs: {");
        outfile.Write("  arr = ");
        outfile.Write("[");
        for (int list_i1 = 0; list_i1 < arr.Count - 1; list_i1++) {
            outfile.Write(arr[list_i1]);
            outfile.Write(", /f ");
        }
        if (arr.Count > 0) {
            outfile.Write(arr[arr.Count - 1]);
        }
        outfile.Write("]");
        outfile.WriteLine(", ");
        outfile.Write("  v = ");
        outfile.WriteLine(v);
        outfile.WriteLine("  }");
        outfile.Close();
        
        for (int i = 0; i < arr.Count - 1; i += 1) {
            if (arr[i] <= v && v <= arr[i + 1]) {
                return i;
            }
        }
        throw new Exception("Bound error");
    }
    
    /** \brief Extracts a column from a 2D matrix
        \param mat matrix from which column will be extracted
        \param j index
        \return column of the given matrix at the given index
    */
    public static List<double> func_extractColumn(List<List<double>> mat, int j) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_extractColumn called with inputs: {");
        outfile.Write("  mat = ");
        outfile.Write("[");
        for (int list_i2 = 0; list_i2 < mat.Count - 1; list_i2++) {
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < mat[list_i2].Count - 1; list_i1++) {
                outfile.Write(mat[list_i2][list_i1]);
                outfile.Write(", /f ");
            }
            if (mat[list_i2].Count > 0) {
                outfile.Write(mat[list_i2][mat[list_i2].Count - 1]);
            }
            outfile.Write("]");
            outfile.Write(", /f ");
        }
        if (mat.Count > 0) {
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < mat[mat.Count - 1].Count - 1; list_i1++) {
                outfile.Write(mat[mat.Count - 1][list_i1]);
                outfile.Write(", /f ");
            }
            if (mat[mat.Count - 1].Count > 0) {
                outfile.Write(mat[mat.Count - 1][mat[mat.Count - 1].Count - 1]);
            }
            outfile.Write("]");
        }
        outfile.Write("]");
        outfile.WriteLine(", ");
        outfile.Write("  j = ");
        outfile.WriteLine(j);
        outfile.WriteLine("  }");
        outfile.Close();
        
        List<double> col = new List<double>(0);
        for (int i = 0; i < mat.Count; i += 1) {
            col.Add(mat[i][j]);
        }
        return col;
    }
    
    /** \brief Linearly interpolates a y value at given x and z values
        \param filename name of file with x y and z data
        \param x x-coordinate to interpolate at
        \param z z-coordinate to interpolate at
        \return y value interpolated at given x and z values
    */
    public static double func_interpY(string filename, double x, double z) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_interpY called with inputs: {");
        outfile.Write("  filename = ");
        outfile.Write(filename);
        outfile.WriteLine(", ");
        outfile.Write("  x = ");
        outfile.Write(x);
        outfile.WriteLine(", ");
        outfile.Write("  z = ");
        outfile.WriteLine(z);
        outfile.WriteLine("  }");
        outfile.Close();
        
        int i;
        List<double> x_z_1;
        List<double> y_z_1;
        List<double> x_z_2;
        List<double> y_z_2;
        int j;
        int k_2;
        double y_1;
        double y_2;
        List<List<double>> x_matrix = new List<List<double>>(0);
        List<List<double>> y_matrix = new List<List<double>>(0);
        List<double> z_vector = new List<double>(0);
        ReadTable.func_read_table(filename, z_vector, x_matrix, y_matrix);
        i = func_find(z_vector, z);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'i' assigned to ");
        outfile.Write(i);
        outfile.WriteLine(" in module Interpolation");
        outfile.Close();
        x_z_1 = func_extractColumn(x_matrix, i);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'x_z_1' assigned to ");
        outfile.Write("[");
        for (int list_i1 = 0; list_i1 < x_z_1.Count - 1; list_i1++) {
            outfile.Write(x_z_1[list_i1]);
            outfile.Write(", /f ");
        }
        if (x_z_1.Count > 0) {
            outfile.Write(x_z_1[x_z_1.Count - 1]);
        }
        outfile.Write("]");
        outfile.WriteLine(" in module Interpolation");
        outfile.Close();
        y_z_1 = func_extractColumn(y_matrix, i);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'y_z_1' assigned to ");
        outfile.Write("[");
        for (int list_i1 = 0; list_i1 < y_z_1.Count - 1; list_i1++) {
            outfile.Write(y_z_1[list_i1]);
            outfile.Write(", /f ");
        }
        if (y_z_1.Count > 0) {
            outfile.Write(y_z_1[y_z_1.Count - 1]);
        }
        outfile.Write("]");
        outfile.WriteLine(" in module Interpolation");
        outfile.Close();
        x_z_2 = func_extractColumn(x_matrix, i + 1);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'x_z_2' assigned to ");
        outfile.Write("[");
        for (int list_i1 = 0; list_i1 < x_z_2.Count - 1; list_i1++) {
            outfile.Write(x_z_2[list_i1]);
            outfile.Write(", /f ");
        }
        if (x_z_2.Count > 0) {
            outfile.Write(x_z_2[x_z_2.Count - 1]);
        }
        outfile.Write("]");
        outfile.WriteLine(" in module Interpolation");
        outfile.Close();
        y_z_2 = func_extractColumn(y_matrix, i + 1);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'y_z_2' assigned to ");
        outfile.Write("[");
        for (int list_i1 = 0; list_i1 < y_z_2.Count - 1; list_i1++) {
            outfile.Write(y_z_2[list_i1]);
            outfile.Write(", /f ");
        }
        if (y_z_2.Count > 0) {
            outfile.Write(y_z_2[y_z_2.Count - 1]);
        }
        outfile.Write("]");
        outfile.WriteLine(" in module Interpolation");
        outfile.Close();
        try {
            j = func_find(x_z_1, x);
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'j' assigned to ");
            outfile.Write(j);
            outfile.WriteLine(" in module Interpolation");
            outfile.Close();
            k_2 = func_find(x_z_2, x);
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'k_2' assigned to ");
            outfile.Write(k_2);
            outfile.WriteLine(" in module Interpolation");
            outfile.Close();
        } catch (Exception exc) {
            throw new Exception("Interpolation of y failed");
        }
        y_1 = func_lin_interp(x_z_1[j], y_z_1[j], x_z_1[j + 1], y_z_1[j + 1], x);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'y_1' assigned to ");
        outfile.Write(y_1);
        outfile.WriteLine(" in module Interpolation");
        outfile.Close();
        y_2 = func_lin_interp(x_z_2[k_2], y_z_2[k_2], x_z_2[k_2 + 1], y_z_2[k_2 + 1], x);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'y_2' assigned to ");
        outfile.Write(y_2);
        outfile.WriteLine(" in module Interpolation");
        outfile.Close();
        return func_lin_interp(z_vector[i], y_1, z_vector[i + 1], y_2, z);
    }
    
    /** \brief Linearly interpolates a z value at given x and y values
        \param filename name of file with x y and z data
        \param x x-coordinate to interpolate at
        \param y y-coordinate to interpolate at
        \return z value interpolated at given x and y values
    */
    public static double func_interpZ(string filename, double x, double y) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_interpZ called with inputs: {");
        outfile.Write("  filename = ");
        outfile.Write(filename);
        outfile.WriteLine(", ");
        outfile.Write("  x = ");
        outfile.Write(x);
        outfile.WriteLine(", ");
        outfile.Write("  y = ");
        outfile.WriteLine(y);
        outfile.WriteLine("  }");
        outfile.Close();
        
        List<double> x_z_1;
        List<double> y_z_1;
        List<double> x_z_2;
        List<double> y_z_2;
        int j;
        int k_2;
        double y_1;
        double y_2;
        List<List<double>> x_matrix = new List<List<double>>(0);
        List<List<double>> y_matrix = new List<List<double>>(0);
        List<double> z_vector = new List<double>(0);
        ReadTable.func_read_table(filename, z_vector, x_matrix, y_matrix);
        for (int i = 0; i < z_vector.Count - 1; i += 1) {
            x_z_1 = func_extractColumn(x_matrix, i);
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'x_z_1' assigned to ");
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < x_z_1.Count - 1; list_i1++) {
                outfile.Write(x_z_1[list_i1]);
                outfile.Write(", /f ");
            }
            if (x_z_1.Count > 0) {
                outfile.Write(x_z_1[x_z_1.Count - 1]);
            }
            outfile.Write("]");
            outfile.WriteLine(" in module Interpolation");
            outfile.Close();
            y_z_1 = func_extractColumn(y_matrix, i);
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'y_z_1' assigned to ");
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < y_z_1.Count - 1; list_i1++) {
                outfile.Write(y_z_1[list_i1]);
                outfile.Write(", /f ");
            }
            if (y_z_1.Count > 0) {
                outfile.Write(y_z_1[y_z_1.Count - 1]);
            }
            outfile.Write("]");
            outfile.WriteLine(" in module Interpolation");
            outfile.Close();
            x_z_2 = func_extractColumn(x_matrix, i + 1);
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'x_z_2' assigned to ");
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < x_z_2.Count - 1; list_i1++) {
                outfile.Write(x_z_2[list_i1]);
                outfile.Write(", /f ");
            }
            if (x_z_2.Count > 0) {
                outfile.Write(x_z_2[x_z_2.Count - 1]);
            }
            outfile.Write("]");
            outfile.WriteLine(" in module Interpolation");
            outfile.Close();
            y_z_2 = func_extractColumn(y_matrix, i + 1);
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'y_z_2' assigned to ");
            outfile.Write("[");
            for (int list_i1 = 0; list_i1 < y_z_2.Count - 1; list_i1++) {
                outfile.Write(y_z_2[list_i1]);
                outfile.Write(", /f ");
            }
            if (y_z_2.Count > 0) {
                outfile.Write(y_z_2[y_z_2.Count - 1]);
            }
            outfile.Write("]");
            outfile.WriteLine(" in module Interpolation");
            outfile.Close();
            try {
                j = func_find(x_z_1, x);
                outfile = new StreamWriter("log.txt", true);
                outfile.Write("var 'j' assigned to ");
                outfile.Write(j);
                outfile.WriteLine(" in module Interpolation");
                outfile.Close();
                k_2 = func_find(x_z_2, x);
                outfile = new StreamWriter("log.txt", true);
                outfile.Write("var 'k_2' assigned to ");
                outfile.Write(k_2);
                outfile.WriteLine(" in module Interpolation");
                outfile.Close();
            } catch (Exception exc) {
                continue;
            }
            y_1 = func_lin_interp(x_z_1[j], y_z_1[j], x_z_1[j + 1], y_z_1[j + 1], x);
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'y_1' assigned to ");
            outfile.Write(y_1);
            outfile.WriteLine(" in module Interpolation");
            outfile.Close();
            y_2 = func_lin_interp(x_z_2[k_2], y_z_2[k_2], x_z_2[k_2 + 1], y_z_2[k_2 + 1], x);
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'y_2' assigned to ");
            outfile.Write(y_2);
            outfile.WriteLine(" in module Interpolation");
            outfile.Close();
            if (y_1 <= y && y <= y_2) {
                return func_lin_interp(y_1, z_vector[i], y_2, z_vector[i + 1], y);
            }
        }
        throw new Exception("Interpolation of z failed");
    }
}

