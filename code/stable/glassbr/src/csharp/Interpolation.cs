using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class Interpolation {
        
        public static double func_lin_interp(double x_1, double y_1, double x_2, double y_2, double x) {
            return (((y_2 - y_1) / (x_2 - x_1)) * (x - x_1)) + y_1;
        }
        
        public static int func_find(List<double> arr, double v) {
            for (int i = 0; i < (arr.Count - 1); i++) {
                if ((arr[i] <= v) && (v <= arr[i + 1])) {
                    return i;
                }
            }
            throw new System.ApplicationException("Bound error");
        }
        
        public static List<double> func_extractColumn(List< List<double> > mat, int j) {
            List<double> col = new List<double>(0);
            for (int i = 0; i < mat.Count; i++) {
                col.Add(mat[i][j]);
            }
            return col;
        }
        
        public static double func_interpY(string filename, double x, double z) {
            int i;
            List<double> x_z_1;
            List<double> y_z_1;
            List<double> x_z_2;
            List<double> y_z_2;
            int j;
            int k_2;
            double y_1;
            double y_2;
            List< List<double> > x_matrix = new List< List<double> >(0);
            List< List<double> > y_matrix = new List< List<double> >(0);
            List<double> z_vector = new List<double>(0);
            ReadTable.func_read_table(filename, z_vector, x_matrix, y_matrix);
            i = func_find(z_vector, z);
            x_z_1 = func_extractColumn(x_matrix, i);
            y_z_1 = func_extractColumn(y_matrix, i);
            x_z_2 = func_extractColumn(x_matrix, i + 1);
            y_z_2 = func_extractColumn(y_matrix, i + 1);
            try {
                j = func_find(x_z_1, x);
                k_2 = func_find(x_z_2, x);
            } catch (System.Exception exc) {
                throw new System.ApplicationException("Interpolation of y failed");
            };
            y_1 = func_lin_interp(x_z_1[j], y_z_1[j], x_z_1[j + 1], y_z_1[j + 1], x);
            y_2 = func_lin_interp(x_z_2[k_2], y_z_2[k_2], x_z_2[k_2 + 1], y_z_2[k_2 + 1], x);
            return func_lin_interp(z_vector[i], y_1, z_vector[i + 1], y_2, z);
        }
        
        public static double func_interpZ(string filename, double x, double y) {
            List<double> x_z_1;
            List<double> y_z_1;
            List<double> x_z_2;
            List<double> y_z_2;
            int j;
            int k_2;
            double y_1;
            double y_2;
            List< List<double> > x_matrix = new List< List<double> >(0);
            List< List<double> > y_matrix = new List< List<double> >(0);
            List<double> z_vector = new List<double>(0);
            ReadTable.func_read_table(filename, z_vector, x_matrix, y_matrix);
            for (int i = 0; i < (z_vector.Count - 1); i++) {
                x_z_1 = func_extractColumn(x_matrix, i);
                y_z_1 = func_extractColumn(y_matrix, i);
                x_z_2 = func_extractColumn(x_matrix, i + 1);
                y_z_2 = func_extractColumn(y_matrix, i + 1);
                try {
                    j = func_find(x_z_1, x);
                    k_2 = func_find(x_z_2, x);
                } catch (System.Exception exc) {
                    continue;
                };
                y_1 = func_lin_interp(x_z_1[j], y_z_1[j], x_z_1[j + 1], y_z_1[j + 1], x);
                y_2 = func_lin_interp(x_z_2[k_2], y_z_2[k_2], x_z_2[k_2 + 1], y_z_2[k_2 + 1], x);
                if ((y_1 <= y) && (y <= y_2)) {
                    return func_lin_interp(y_1, z_vector[i], y_2, z_vector[i + 1], y);
                }
            }
            throw new System.ApplicationException("Interpolation of z failed");
        }
    }
}

