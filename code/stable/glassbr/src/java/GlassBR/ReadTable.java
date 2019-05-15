package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class ReadTable {
    
    public static void func_read_table(String filename, Vector<Double> z_vector, Vector< Vector<Double> > x_matrix, Vector< Vector<Double> > y_matrix) throws Exception {
        Scanner infile;
        String line;
        Vector<String> lines = new Vector<String>(0);
        Vector<String> linetokens = new Vector<String>(0);
        infile = new Scanner(new File(filename));
        line = infile.nextLine();
        linetokens = new Vector<String>(Arrays.asList(line.split(",")));
        for (int j = 0; j < (int)(linetokens.size() / 2); j++) {
            while (z_vector.size() <= j) {
                z_vector.add(0.0);
            }
            z_vector.set(j, Double.parseDouble(linetokens.get((j * 2) + 1)));
        }
        while (infile.hasNextLine()) {
            lines.add(infile.nextLine());
        }
        for (int i = 0; i < lines.size(); i++) {
            linetokens = new Vector<String>(Arrays.asList(lines.get(i).split(",")));
            for (int j = 0; j < (int)(linetokens.size() / 2); j++) {
                while (x_matrix.size() <= i) {
                    x_matrix.add(new Vector<Double>());
                }
                while (x_matrix.get(i).size() <= j) {
                    x_matrix.get(i).add(0.0);
                }
                x_matrix.get(i).set(j, Double.parseDouble(linetokens.get((j * 2) + 0)));
                while (y_matrix.size() <= i) {
                    y_matrix.add(new Vector<Double>());
                }
                while (y_matrix.get(i).size() <= j) {
                    y_matrix.get(i).add(0.0);
                }
                y_matrix.get(i).set(j, Double.parseDouble(linetokens.get((j * 2) + 1)));
            }
        }
        infile.close();
    }
}

