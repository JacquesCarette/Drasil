using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class Interpolation {
        
        public static double lin_interp(double x1, double y1, double x2, double y2, double x) {
            double y = (((y2 - y1) / (x2 - x1)) * (x - x1)) + y1;
            return y;
        }
        
        public static int indInSeq(List<double> arr, double v) {
            for (int i = 0; i < (arr.Count - 1); i++) {
                if ((arr[i] <= v) && (v <= arr[i + 1])) {
                    return i;
                }
            }
            throw new System.ApplicationException("Index not found");
        }
        
        public static List<double> matrixCol(List< List<double> > mat, int c) {
            List<double> col = new List<double>(0);
            for (int i = 0; i < mat.Count; i++) {
                col.Add(mat[i][c]);
            }
            return col;
        }
        
        public static double interpY(List< List<double> > x_array, List< List<double> > y_array, List<double> z_array, double x, double z) {
            int i = indInSeq(z_array, z);
            List<double> x_z1 = matrixCol(x_array, i);
            List<double> y_z1 = matrixCol(y_array, i);
            List<double> x_z2 = matrixCol(x_array, i + 1);
            List<double> y_z2 = matrixCol(y_array, i + 1);
            int j;
            int k;
            try {
                j = indInSeq(x_z1, x);
                k = indInSeq(x_z2, x);
            } catch (System.Exception exc) {
                throw new System.ApplicationException("Interpolation of y failed.");
            };
            double y1 = lin_interp(x_z1[j], y_z1[j], x_z1[j + 1], y_z1[j + 1], x);
            double y2 = lin_interp(x_z2[k], y_z2[k], x_z2[k + 1], y_z2[k + 1], x);
            return lin_interp(z_array[i], y1, z_array[i + 1], y2, z);
        }
        
        public static double interpZ(List< List<double> > x_array, List< List<double> > y_array, List<double> z_array, double x, double y) {
            for (int i = 0; i < (z_array.Count - 1); i++) {
                List<double> x_z1 = matrixCol(x_array, i);
                List<double> y_z1 = matrixCol(y_array, i);
                List<double> x_z2 = matrixCol(x_array, i + 1);
                List<double> y_z2 = matrixCol(y_array, i + 1);
                int j;
                int k;
                try {
                    j = indInSeq(x_z1, x);
                    k = indInSeq(x_z2, x);
                } catch (System.Exception exc) {
                    continue;
                };
                double y_lower = lin_interp(x_z1[j], y_z1[j], x_z1[j + 1], y_z1[j + 1], x);
                double y_upper = lin_interp(x_z2[k], y_z2[k], x_z2[k + 1], y_z2[k + 1], x);
                if ((y_lower <= y) && (y <= y_upper)) {
                    return lin_interp(y_lower, z_array[i], y_upper, z_array[i + 1], y);
                }
            }
            throw new System.ApplicationException("Interpolation of z failed.");
        }
    }
}

