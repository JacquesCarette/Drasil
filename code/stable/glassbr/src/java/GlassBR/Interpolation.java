package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class Interpolation {
    
    public static double func_lin_interp(double x_1, double y_1, double x_2, double y_2, double x) throws Exception {
        return ((((y_2 - y_1) / (x_2 - x_1)) * (x - x_1)) + y_1);
    }
    
    public static int func_find(ArrayList<Double> arr, double v) throws Exception {
        for (int i = 0; (i < (arr.size() - 1)); i += 1) {
            if (((arr.get(i) <= v) && (v <= arr.get((i + 1))))) {
                return i;
            }
        }
        throw new Exception("Bound error");
    }
    
    public static ArrayList<Double> func_extractColumn(ArrayList<ArrayList<Double>> mat, int j) throws Exception {
        ArrayList<Double> col = new ArrayList<Double>(0);
        for (int i = 0; (i < mat.size()); i += 1) {
            col.add(mat.get(i).get(j));
        }
        return col;
    }
    
    public static double func_interpY(String filename, double x, double z) throws Exception {
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
        x_z_1 = func_extractColumn(x_matrix, i);
        y_z_1 = func_extractColumn(y_matrix, i);
        x_z_2 = func_extractColumn(x_matrix, (i + 1));
        y_z_2 = func_extractColumn(y_matrix, (i + 1));
        try {
            j = func_find(x_z_1, x);
            k_2 = func_find(x_z_2, x);
        } catch (Exception exc) {
            throw new Exception("Interpolation of y failed");
        }
        y_1 = func_lin_interp(x_z_1.get(j), y_z_1.get(j), x_z_1.get((j + 1)), y_z_1.get((j + 1)), x);
        y_2 = func_lin_interp(x_z_2.get(k_2), y_z_2.get(k_2), x_z_2.get((k_2 + 1)), y_z_2.get((k_2 + 1)), x);
        return func_lin_interp(z_vector.get(i), y_1, z_vector.get((i + 1)), y_2, z);
    }
    
    public static double func_interpZ(String filename, double x, double y) throws Exception {
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
        for (int i = 0; (i < (z_vector.size() - 1)); i += 1) {
            x_z_1 = func_extractColumn(x_matrix, i);
            y_z_1 = func_extractColumn(y_matrix, i);
            x_z_2 = func_extractColumn(x_matrix, (i + 1));
            y_z_2 = func_extractColumn(y_matrix, (i + 1));
            try {
                j = func_find(x_z_1, x);
                k_2 = func_find(x_z_2, x);
            } catch (Exception exc) {
                continue;
            }
            y_1 = func_lin_interp(x_z_1.get(j), y_z_1.get(j), x_z_1.get((j + 1)), y_z_1.get((j + 1)), x);
            y_2 = func_lin_interp(x_z_2.get(k_2), y_z_2.get(k_2), x_z_2.get((k_2 + 1)), y_z_2.get((k_2 + 1)), x);
            if (((y_1 <= y) && (y <= y_2))) {
                return func_lin_interp(y_1, z_vector.get(i), y_2, z_vector.get((i + 1)), y);
            }
        }
        throw new Exception("Interpolation of z failed");
    }
}

