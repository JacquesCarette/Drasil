package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class Interpolation {
    
    public static double lin_interp(double x1, double y1, double x2, double y2, double x) {
        double y = (((y2 - y1) / (x2 - x1)) * (x - x1)) + y1;
        return y;
    }
    
    public static int indInSeq(Vector<Double> arr, double v) throws Exception {
        for (int i = 0; i < (arr.size() - 1); i++) {
            if ((arr.get(i) <= v) && (v <= arr.get(i + 1))) {
                return i;
            }
        }
        throw new Exception("Index not found");
    }
    
    public static Vector<Double> matrixCol(Vector< Vector<Double> > mat, int c) {
        Vector<Double> col = new Vector<Double>(0);
        for (int i = 0; i < mat.size(); i++) {
            col.add(mat.get(i).get(c));
        }
        return col;
    }
    
    public static double interpY(Vector< Vector<Double> > x_array, Vector< Vector<Double> > y_array, Vector<Double> z_array, double x, double z) throws Exception {
        int i = indInSeq(z_array, z);
        Vector<Double> x_z1 = matrixCol(x_array, i);
        Vector<Double> y_z1 = matrixCol(y_array, i);
        Vector<Double> x_z2 = matrixCol(x_array, i + 1);
        Vector<Double> y_z2 = matrixCol(y_array, i + 1);
        int j;
        int k;
        try {
            j = indInSeq(x_z1, x);
            k = indInSeq(x_z2, x);
        } catch (Exception exc) {
            throw new Exception("Interpolation of y failed.");
        };
        double y1 = lin_interp(x_z1.get(j), y_z1.get(j), x_z1.get(j + 1), y_z1.get(j + 1), x);
        double y2 = lin_interp(x_z2.get(k), y_z2.get(k), x_z2.get(k + 1), y_z2.get(k + 1), x);
        return lin_interp(z_array.get(i), y1, z_array.get(i + 1), y2, z);
    }
    
    public static double interpZ(Vector< Vector<Double> > x_array, Vector< Vector<Double> > y_array, Vector<Double> z_array, double x, double y) throws Exception {
        for (int i = 0; i < (z_array.size() - 1); i++) {
            Vector<Double> x_z1 = matrixCol(x_array, i);
            Vector<Double> y_z1 = matrixCol(y_array, i);
            Vector<Double> x_z2 = matrixCol(x_array, i + 1);
            Vector<Double> y_z2 = matrixCol(y_array, i + 1);
            int j;
            int k;
            try {
                j = indInSeq(x_z1, x);
                k = indInSeq(x_z2, x);
            } catch (Exception exc) {
                continue;
            };
            double y_lower = lin_interp(x_z1.get(j), y_z1.get(j), x_z1.get(j + 1), y_z1.get(j + 1), x);
            double y_upper = lin_interp(x_z2.get(k), y_z2.get(k), x_z2.get(k + 1), y_z2.get(k + 1), x);
            if ((y_lower <= y) && (y <= y_upper)) {
                return lin_interp(y_lower, z_array.get(i), y_upper, z_array.get(i + 1), y);
            }
        }
        throw new Exception("Interpolation of z failed.");
    }
}

