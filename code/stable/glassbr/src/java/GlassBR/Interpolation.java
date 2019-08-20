package GlassBR;

/** \file Interpolation.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides functions for linear interpolation on three-dimensional data
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class Interpolation {
    
    /** \brief Performs linear interpolation
        \param x_1 lower x-coordinate
        \param y_1 lower y-coordinate
        \param x_2 upper x-coordinate
        \param y_2 upper y-coordinate
        \param x x-coordinate to interpolate at
        \return y value interpolated at given x value
    */
    public static double func_lin_interp(double x_1, double y_1, double x_2, double y_2, double x) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_lin_interp called with inputs: {");
        outfile.print("  x_1 = ");
        outfile.print(x_1);
        outfile.println(", ");
        outfile.print("  y_1 = ");
        outfile.print(y_1);
        outfile.println(", ");
        outfile.print("  x_2 = ");
        outfile.print(x_2);
        outfile.println(", ");
        outfile.print("  y_2 = ");
        outfile.print(y_2);
        outfile.println(", ");
        outfile.print("  x = ");
        outfile.println(x);
        outfile.println("  }");
        outfile.close();
        
        return (y_2 - y_1) / (x_2 - x_1) * (x - x_1) + y_1;
    }
    
    /** \brief Finds the array index for a value closest to the given value
        \param arr array in which value should be found
        \param v value whose index will be found
        \return index of given value in given array
    */
    public static int func_find(ArrayList<Double> arr, double v) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_find called with inputs: {");
        outfile.print("  arr = ");
        outfile.print("[");
        for (int list_i1 = 0; list_i1 < arr.size() - 1; list_i1++) {
            outfile.print(arr.get(list_i1));
            outfile.print(", /f ");
        }
        if (arr.size() > 0) {
            outfile.print(arr.get(arr.size() - 1));
        }
        outfile.print("]");
        outfile.println(", ");
        outfile.print("  v = ");
        outfile.println(v);
        outfile.println("  }");
        outfile.close();
        
        for (int i = 0; i < arr.size() - 1; i += 1) {
            if (arr.get(i) <= v && v <= arr.get(i + 1)) {
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
    public static ArrayList<Double> func_extractColumn(ArrayList<ArrayList<Double>> mat, int j) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_extractColumn called with inputs: {");
        outfile.print("  mat = ");
        outfile.print("[");
        for (int list_i2 = 0; list_i2 < mat.size() - 1; list_i2++) {
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < mat.get(list_i2).size() - 1; list_i1++) {
                outfile.print(mat.get(list_i2).get(list_i1));
                outfile.print(", /f ");
            }
            if (mat.get(list_i2).size() > 0) {
                outfile.print(mat.get(list_i2).get(mat.get(list_i2).size() - 1));
            }
            outfile.print("]");
            outfile.print(", /f ");
        }
        if (mat.size() > 0) {
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < mat.get(mat.size() - 1).size() - 1; list_i1++) {
                outfile.print(mat.get(mat.size() - 1).get(list_i1));
                outfile.print(", /f ");
            }
            if (mat.get(mat.size() - 1).size() > 0) {
                outfile.print(mat.get(mat.size() - 1).get(mat.get(mat.size() - 1).size() - 1));
            }
            outfile.print("]");
        }
        outfile.print("]");
        outfile.println(", ");
        outfile.print("  j = ");
        outfile.println(j);
        outfile.println("  }");
        outfile.close();
        
        ArrayList<Double> col = new ArrayList<Double>(0);
        for (int i = 0; i < mat.size(); i += 1) {
            col.add(mat.get(i).get(j));
        }
        return col;
    }
    
    /** \brief Linearly interpolates a y value at given x and z values
        \param filename name of file with x y and z data
        \param x x-coordinate to interpolate at
        \param z z-coordinate to interpolate at
        \return y value interpolated at given x and z values
    */
    public static double func_interpY(String filename, double x, double z) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_interpY called with inputs: {");
        outfile.print("  filename = ");
        outfile.print(filename);
        outfile.println(", ");
        outfile.print("  x = ");
        outfile.print(x);
        outfile.println(", ");
        outfile.print("  z = ");
        outfile.println(z);
        outfile.println("  }");
        outfile.close();
        
        int i;
        ArrayList<Double> x_z_1;
        ArrayList<Double> y_z_1;
        ArrayList<Double> x_z_2;
        ArrayList<Double> y_z_2;
        int j;
        int k_2;
        double y_1;
        double y_2;
        ArrayList<ArrayList<Double>> x_matrix = new ArrayList<ArrayList<Double>>(0);
        ArrayList<ArrayList<Double>> y_matrix = new ArrayList<ArrayList<Double>>(0);
        ArrayList<Double> z_vector = new ArrayList<Double>(0);
        ReadTable.func_read_table(filename, z_vector, x_matrix, y_matrix);
        i = func_find(z_vector, z);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'i' assigned to ");
        outfile.print(i);
        outfile.println(" in module Interpolation");
        outfile.close();
        x_z_1 = func_extractColumn(x_matrix, i);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'x_z_1' assigned to ");
        outfile.print("[");
        for (int list_i1 = 0; list_i1 < x_z_1.size() - 1; list_i1++) {
            outfile.print(x_z_1.get(list_i1));
            outfile.print(", /f ");
        }
        if (x_z_1.size() > 0) {
            outfile.print(x_z_1.get(x_z_1.size() - 1));
        }
        outfile.print("]");
        outfile.println(" in module Interpolation");
        outfile.close();
        y_z_1 = func_extractColumn(y_matrix, i);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'y_z_1' assigned to ");
        outfile.print("[");
        for (int list_i1 = 0; list_i1 < y_z_1.size() - 1; list_i1++) {
            outfile.print(y_z_1.get(list_i1));
            outfile.print(", /f ");
        }
        if (y_z_1.size() > 0) {
            outfile.print(y_z_1.get(y_z_1.size() - 1));
        }
        outfile.print("]");
        outfile.println(" in module Interpolation");
        outfile.close();
        x_z_2 = func_extractColumn(x_matrix, i + 1);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'x_z_2' assigned to ");
        outfile.print("[");
        for (int list_i1 = 0; list_i1 < x_z_2.size() - 1; list_i1++) {
            outfile.print(x_z_2.get(list_i1));
            outfile.print(", /f ");
        }
        if (x_z_2.size() > 0) {
            outfile.print(x_z_2.get(x_z_2.size() - 1));
        }
        outfile.print("]");
        outfile.println(" in module Interpolation");
        outfile.close();
        y_z_2 = func_extractColumn(y_matrix, i + 1);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'y_z_2' assigned to ");
        outfile.print("[");
        for (int list_i1 = 0; list_i1 < y_z_2.size() - 1; list_i1++) {
            outfile.print(y_z_2.get(list_i1));
            outfile.print(", /f ");
        }
        if (y_z_2.size() > 0) {
            outfile.print(y_z_2.get(y_z_2.size() - 1));
        }
        outfile.print("]");
        outfile.println(" in module Interpolation");
        outfile.close();
        try {
            j = func_find(x_z_1, x);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'j' assigned to ");
            outfile.print(j);
            outfile.println(" in module Interpolation");
            outfile.close();
            k_2 = func_find(x_z_2, x);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'k_2' assigned to ");
            outfile.print(k_2);
            outfile.println(" in module Interpolation");
            outfile.close();
        } catch (Exception exc) {
            throw new Exception("Interpolation of y failed");
        }
        y_1 = func_lin_interp(x_z_1.get(j), y_z_1.get(j), x_z_1.get(j + 1), y_z_1.get(j + 1), x);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'y_1' assigned to ");
        outfile.print(y_1);
        outfile.println(" in module Interpolation");
        outfile.close();
        y_2 = func_lin_interp(x_z_2.get(k_2), y_z_2.get(k_2), x_z_2.get(k_2 + 1), y_z_2.get(k_2 + 1), x);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'y_2' assigned to ");
        outfile.print(y_2);
        outfile.println(" in module Interpolation");
        outfile.close();
        return func_lin_interp(z_vector.get(i), y_1, z_vector.get(i + 1), y_2, z);
    }
    
    /** \brief Linearly interpolates a z value at given x and y values
        \param filename name of file with x y and z data
        \param x x-coordinate to interpolate at
        \param y y-coordinate to interpolate at
        \return z value interpolated at given x and y values
    */
    public static double func_interpZ(String filename, double x, double y) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_interpZ called with inputs: {");
        outfile.print("  filename = ");
        outfile.print(filename);
        outfile.println(", ");
        outfile.print("  x = ");
        outfile.print(x);
        outfile.println(", ");
        outfile.print("  y = ");
        outfile.println(y);
        outfile.println("  }");
        outfile.close();
        
        ArrayList<Double> x_z_1;
        ArrayList<Double> y_z_1;
        ArrayList<Double> x_z_2;
        ArrayList<Double> y_z_2;
        int j;
        int k_2;
        double y_1;
        double y_2;
        ArrayList<ArrayList<Double>> x_matrix = new ArrayList<ArrayList<Double>>(0);
        ArrayList<ArrayList<Double>> y_matrix = new ArrayList<ArrayList<Double>>(0);
        ArrayList<Double> z_vector = new ArrayList<Double>(0);
        ReadTable.func_read_table(filename, z_vector, x_matrix, y_matrix);
        for (int i = 0; i < z_vector.size() - 1; i += 1) {
            x_z_1 = func_extractColumn(x_matrix, i);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'x_z_1' assigned to ");
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < x_z_1.size() - 1; list_i1++) {
                outfile.print(x_z_1.get(list_i1));
                outfile.print(", /f ");
            }
            if (x_z_1.size() > 0) {
                outfile.print(x_z_1.get(x_z_1.size() - 1));
            }
            outfile.print("]");
            outfile.println(" in module Interpolation");
            outfile.close();
            y_z_1 = func_extractColumn(y_matrix, i);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'y_z_1' assigned to ");
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < y_z_1.size() - 1; list_i1++) {
                outfile.print(y_z_1.get(list_i1));
                outfile.print(", /f ");
            }
            if (y_z_1.size() > 0) {
                outfile.print(y_z_1.get(y_z_1.size() - 1));
            }
            outfile.print("]");
            outfile.println(" in module Interpolation");
            outfile.close();
            x_z_2 = func_extractColumn(x_matrix, i + 1);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'x_z_2' assigned to ");
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < x_z_2.size() - 1; list_i1++) {
                outfile.print(x_z_2.get(list_i1));
                outfile.print(", /f ");
            }
            if (x_z_2.size() > 0) {
                outfile.print(x_z_2.get(x_z_2.size() - 1));
            }
            outfile.print("]");
            outfile.println(" in module Interpolation");
            outfile.close();
            y_z_2 = func_extractColumn(y_matrix, i + 1);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'y_z_2' assigned to ");
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < y_z_2.size() - 1; list_i1++) {
                outfile.print(y_z_2.get(list_i1));
                outfile.print(", /f ");
            }
            if (y_z_2.size() > 0) {
                outfile.print(y_z_2.get(y_z_2.size() - 1));
            }
            outfile.print("]");
            outfile.println(" in module Interpolation");
            outfile.close();
            try {
                j = func_find(x_z_1, x);
                outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
                outfile.print("var 'j' assigned to ");
                outfile.print(j);
                outfile.println(" in module Interpolation");
                outfile.close();
                k_2 = func_find(x_z_2, x);
                outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
                outfile.print("var 'k_2' assigned to ");
                outfile.print(k_2);
                outfile.println(" in module Interpolation");
                outfile.close();
            } catch (Exception exc) {
                continue;
            }
            y_1 = func_lin_interp(x_z_1.get(j), y_z_1.get(j), x_z_1.get(j + 1), y_z_1.get(j + 1), x);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'y_1' assigned to ");
            outfile.print(y_1);
            outfile.println(" in module Interpolation");
            outfile.close();
            y_2 = func_lin_interp(x_z_2.get(k_2), y_z_2.get(k_2), x_z_2.get(k_2 + 1), y_z_2.get(k_2 + 1), x);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'y_2' assigned to ");
            outfile.print(y_2);
            outfile.println(" in module Interpolation");
            outfile.close();
            if (y_1 <= y && y <= y_2) {
                return func_lin_interp(y_1, z_vector.get(i), y_2, z_vector.get(i + 1), y);
            }
        }
        throw new Exception("Interpolation of z failed");
    }
}

