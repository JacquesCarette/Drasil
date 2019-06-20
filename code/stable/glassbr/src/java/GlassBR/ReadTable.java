package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class ReadTable {
    
    public static void func_read_table(String filename, ArrayList<Double> z_vector, ArrayList<ArrayList<Double>> x_matrix, ArrayList<ArrayList<Double>> y_matrix) throws Exception {
        Scanner infile;
        String line;
        ArrayList<String> lines = new ArrayList<String>(0);
        ArrayList<String> linetokens = new ArrayList<String>(0);
        infile = new Scanner(new File(filename));
        line = infile.nextLine();
        linetokens = new ArrayList<String>(Arrays.asList(line.split(",")));
        for (int j = 0; (j < (int)((linetokens.size() / 2))); j += 1) {
            z_vector.add(Double.parseDouble(linetokens.get(((j * 2) + 1))));
        }
        while (infile.hasNextLine()) {
            lines.add(infile.nextLine());
        }
        for (int i = 0; (i < lines.size()); i += 1) {
            linetokens = new ArrayList<String>(Arrays.asList(lines.get(i).split(",")));
            ArrayList<Double> x_matrix_temp = new ArrayList<Double> ();
            ArrayList<Double> y_matrix_temp = new ArrayList<Double> ();
            for (int j = 0; (j < (int)((linetokens.size() / 2))); j += 1) {
                x_matrix_temp.add(Double.parseDouble(linetokens.get(((j * 2) + 0))));
                y_matrix_temp.add(Double.parseDouble(linetokens.get(((j * 2) + 1))));
            }
            x_matrix.add(x_matrix_temp);
            y_matrix.add(y_matrix_temp);
        }
        infile.close();
    }
}

