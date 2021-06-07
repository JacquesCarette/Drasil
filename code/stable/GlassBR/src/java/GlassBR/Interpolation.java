package GlassBR;

/** \file Interpolation.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides functions for linear interpolation on three-dimensional data
*/
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
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
    public static double lin_interp(double x_1, double y_1, double x_2, double y_2, double x) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function lin_interp called with inputs: {");
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
    public static int find(ArrayList<Double> arr, double v) throws Exception, IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function find called with inputs: {");
        outfile.print("  arr = ");
        outfile.print(arr);
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
    public static ArrayList<Double> extractColumn(ArrayList<ArrayList<Double>> mat, int j) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function extractColumn called with inputs: {");
        outfile.print("  mat = ");
        outfile.print(mat);
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
    public static double interpY(String filename, double x, double z) throws Exception, FileNotFoundException, IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function interpY called with inputs: {");
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
        ReadTable.read_table(filename, z_vector, x_matrix, y_matrix);
        i = find(z_vector, z);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'i' assigned ");
        outfile.print(i);
        outfile.println(" in module Interpolation");
        outfile.close();
        x_z_1 = extractColumn(x_matrix, i);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'x_z_1' assigned ");
        outfile.print(x_z_1);
        outfile.println(" in module Interpolation");
        outfile.close();
        y_z_1 = extractColumn(y_matrix, i);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'y_z_1' assigned ");
        outfile.print(y_z_1);
        outfile.println(" in module Interpolation");
        outfile.close();
        x_z_2 = extractColumn(x_matrix, i + 1);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'x_z_2' assigned ");
        outfile.print(x_z_2);
        outfile.println(" in module Interpolation");
        outfile.close();
        y_z_2 = extractColumn(y_matrix, i + 1);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'y_z_2' assigned ");
        outfile.print(y_z_2);
        outfile.println(" in module Interpolation");
        outfile.close();
        try {
            j = find(x_z_1, x);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'j' assigned ");
            outfile.print(j);
            outfile.println(" in module Interpolation");
            outfile.close();
            k_2 = find(x_z_2, x);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'k_2' assigned ");
            outfile.print(k_2);
            outfile.println(" in module Interpolation");
            outfile.close();
        } catch (Exception exc) {
            throw new Exception("Interpolation of y failed");
        }
        y_1 = lin_interp(x_z_1.get(j), y_z_1.get(j), x_z_1.get(j + 1), y_z_1.get(j + 1), x);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'y_1' assigned ");
        outfile.print(y_1);
        outfile.println(" in module Interpolation");
        outfile.close();
        y_2 = lin_interp(x_z_2.get(k_2), y_z_2.get(k_2), x_z_2.get(k_2 + 1), y_z_2.get(k_2 + 1), x);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'y_2' assigned ");
        outfile.print(y_2);
        outfile.println(" in module Interpolation");
        outfile.close();
        return lin_interp(z_vector.get(i), y_1, z_vector.get(i + 1), y_2, z);
    }
    
    /** \brief Linearly interpolates a z value at given x and y values
        \param filename name of file with x y and z data
        \param x x-coordinate to interpolate at
        \param y y-coordinate to interpolate at
        \return z value interpolated at given x and y values
    */
    public static double interpZ(String filename, double x, double y) throws Exception, FileNotFoundException, IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function interpZ called with inputs: {");
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
        ReadTable.read_table(filename, z_vector, x_matrix, y_matrix);
        for (int i = 0; i < z_vector.size() - 1; i += 1) {
            x_z_1 = extractColumn(x_matrix, i);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'x_z_1' assigned ");
            outfile.print(x_z_1);
            outfile.println(" in module Interpolation");
            outfile.close();
            y_z_1 = extractColumn(y_matrix, i);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'y_z_1' assigned ");
            outfile.print(y_z_1);
            outfile.println(" in module Interpolation");
            outfile.close();
            x_z_2 = extractColumn(x_matrix, i + 1);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'x_z_2' assigned ");
            outfile.print(x_z_2);
            outfile.println(" in module Interpolation");
            outfile.close();
            y_z_2 = extractColumn(y_matrix, i + 1);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'y_z_2' assigned ");
            outfile.print(y_z_2);
            outfile.println(" in module Interpolation");
            outfile.close();
            try {
                j = find(x_z_1, x);
                outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
                outfile.print("var 'j' assigned ");
                outfile.print(j);
                outfile.println(" in module Interpolation");
                outfile.close();
                k_2 = find(x_z_2, x);
                outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
                outfile.print("var 'k_2' assigned ");
                outfile.print(k_2);
                outfile.println(" in module Interpolation");
                outfile.close();
            } catch (Exception exc) {
                continue;
            }
            y_1 = lin_interp(x_z_1.get(j), y_z_1.get(j), x_z_1.get(j + 1), y_z_1.get(j + 1), x);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'y_1' assigned ");
            outfile.print(y_1);
            outfile.println(" in module Interpolation");
            outfile.close();
            y_2 = lin_interp(x_z_2.get(k_2), y_z_2.get(k_2), x_z_2.get(k_2 + 1), y_z_2.get(k_2 + 1), x);
            outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
            outfile.print("var 'y_2' assigned ");
            outfile.print(y_2);
            outfile.println(" in module Interpolation");
            outfile.close();
            if (y_1 <= y && y <= y_2) {
                return lin_interp(y_1, z_vector.get(i), y_2, z_vector.get(i + 1), y);
            }
        }
        throw new Exception("Interpolation of z failed");
    }
}
