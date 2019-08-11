package GlassBR;

/** \file ReadTable.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides a function for reading glass ASTM data
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class ReadTable {
    
    /** \brief Reads glass ASTM data from a file with the given file name
        \param filename name of file with x y and z data
        \param z_vector list of z values
        \param x_matrix lists of x values at different z values
        \param y_matrix lists of y values at different z values
    */
    public static void func_read_table(String filename, ArrayList<Double> z_vector, ArrayList<ArrayList<Double>> x_matrix, ArrayList<ArrayList<Double>> y_matrix) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_read_table called with inputs: {");
        outfile.print("  filename = ");
        outfile.print(filename);
        outfile.println(", ");
        outfile.print("  z_vector = ");
        outfile.print("[");
        for (int list_i1 = 0; list_i1 < z_vector.size() - 1; list_i1++) {
            outfile.print(z_vector.get(list_i1));
            outfile.print(", /f ");
        }
        if (z_vector.size() > 0) {
            outfile.print(z_vector.get(z_vector.size() - 1));
        }
        outfile.print("]");
        outfile.println(", ");
        outfile.print("  x_matrix = ");
        outfile.print("[");
        for (int list_i2 = 0; list_i2 < x_matrix.size() - 1; list_i2++) {
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < x_matrix.get(list_i2).size() - 1; list_i1++) {
                outfile.print(x_matrix.get(list_i2).get(list_i1));
                outfile.print(", /f ");
            }
            if (x_matrix.get(list_i2).size() > 0) {
                outfile.print(x_matrix.get(list_i2).get(x_matrix.get(list_i2).size() - 1));
            }
            outfile.print("]");
            outfile.print(", /f ");
        }
        if (x_matrix.size() > 0) {
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < x_matrix.get(x_matrix.size() - 1).size() - 1; list_i1++) {
                outfile.print(x_matrix.get(x_matrix.size() - 1).get(list_i1));
                outfile.print(", /f ");
            }
            if (x_matrix.get(x_matrix.size() - 1).size() > 0) {
                outfile.print(x_matrix.get(x_matrix.size() - 1).get(x_matrix.get(x_matrix.size() - 1).size() - 1));
            }
            outfile.print("]");
        }
        outfile.print("]");
        outfile.println(", ");
        outfile.print("  y_matrix = ");
        outfile.print("[");
        for (int list_i2 = 0; list_i2 < y_matrix.size() - 1; list_i2++) {
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < y_matrix.get(list_i2).size() - 1; list_i1++) {
                outfile.print(y_matrix.get(list_i2).get(list_i1));
                outfile.print(", /f ");
            }
            if (y_matrix.get(list_i2).size() > 0) {
                outfile.print(y_matrix.get(list_i2).get(y_matrix.get(list_i2).size() - 1));
            }
            outfile.print("]");
            outfile.print(", /f ");
        }
        if (y_matrix.size() > 0) {
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < y_matrix.get(y_matrix.size() - 1).size() - 1; list_i1++) {
                outfile.print(y_matrix.get(y_matrix.size() - 1).get(list_i1));
                outfile.print(", /f ");
            }
            if (y_matrix.get(y_matrix.size() - 1).size() > 0) {
                outfile.print(y_matrix.get(y_matrix.size() - 1).get(y_matrix.get(y_matrix.size() - 1).size() - 1));
            }
            outfile.print("]");
        }
        outfile.println("]");
        outfile.println("  }");
        outfile.close();
        
        Scanner infile;
        String line;
        ArrayList<String> linetokens = new ArrayList<String>(0);
        ArrayList<String> lines = new ArrayList<String>(0);
        infile = new Scanner(new File(filename));
        line = infile.nextLine();
        linetokens = new ArrayList<String>(Arrays.asList(line.split(",")));
        for (int stringlist_i = 0; stringlist_i < linetokens.size() / 1; stringlist_i += 1) {
            z_vector.add(Double.parseDouble(linetokens.get(stringlist_i * 1 + 0)));
        }
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'z_vector' assigned to ");
        outfile.print("[");
        for (int list_i1 = 0; list_i1 < z_vector.size() - 1; list_i1++) {
            outfile.print(z_vector.get(list_i1));
            outfile.print(", /f ");
        }
        if (z_vector.size() > 0) {
            outfile.print(z_vector.get(z_vector.size() - 1));
        }
        outfile.print("]");
        outfile.println(" in module ReadTable");
        outfile.close();
        while (infile.hasNextLine()) {
            lines.add(infile.nextLine());
        }
        for (int i = 0; i < lines.size(); i += 1) {
            linetokens = new ArrayList<String>(Arrays.asList(lines.get(i).split(",")));
            ArrayList<Double> x_matrix_temp = new ArrayList<Double> ();
            ArrayList<Double> y_matrix_temp = new ArrayList<Double> ();
            for (int stringlist_i = 0; stringlist_i < linetokens.size() / 2; stringlist_i += 1) {
                x_matrix_temp.add(Double.parseDouble(linetokens.get(stringlist_i * 2 + 0)));
                y_matrix_temp.add(Double.parseDouble(linetokens.get(stringlist_i * 2 + 1)));
            }
            x_matrix.add(x_matrix_temp);
            y_matrix.add(y_matrix_temp);
        }
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'x_matrix' assigned to ");
        outfile.print("[");
        for (int list_i2 = 0; list_i2 < x_matrix.size() - 1; list_i2++) {
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < x_matrix.get(list_i2).size() - 1; list_i1++) {
                outfile.print(x_matrix.get(list_i2).get(list_i1));
                outfile.print(", /f ");
            }
            if (x_matrix.get(list_i2).size() > 0) {
                outfile.print(x_matrix.get(list_i2).get(x_matrix.get(list_i2).size() - 1));
            }
            outfile.print("]");
            outfile.print(", /f ");
        }
        if (x_matrix.size() > 0) {
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < x_matrix.get(x_matrix.size() - 1).size() - 1; list_i1++) {
                outfile.print(x_matrix.get(x_matrix.size() - 1).get(list_i1));
                outfile.print(", /f ");
            }
            if (x_matrix.get(x_matrix.size() - 1).size() > 0) {
                outfile.print(x_matrix.get(x_matrix.size() - 1).get(x_matrix.get(x_matrix.size() - 1).size() - 1));
            }
            outfile.print("]");
        }
        outfile.print("]");
        outfile.println(" in module ReadTable");
        outfile.close();
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'y_matrix' assigned to ");
        outfile.print("[");
        for (int list_i2 = 0; list_i2 < y_matrix.size() - 1; list_i2++) {
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < y_matrix.get(list_i2).size() - 1; list_i1++) {
                outfile.print(y_matrix.get(list_i2).get(list_i1));
                outfile.print(", /f ");
            }
            if (y_matrix.get(list_i2).size() > 0) {
                outfile.print(y_matrix.get(list_i2).get(y_matrix.get(list_i2).size() - 1));
            }
            outfile.print("]");
            outfile.print(", /f ");
        }
        if (y_matrix.size() > 0) {
            outfile.print("[");
            for (int list_i1 = 0; list_i1 < y_matrix.get(y_matrix.size() - 1).size() - 1; list_i1++) {
                outfile.print(y_matrix.get(y_matrix.size() - 1).get(list_i1));
                outfile.print(", /f ");
            }
            if (y_matrix.get(y_matrix.size() - 1).size() > 0) {
                outfile.print(y_matrix.get(y_matrix.size() - 1).get(y_matrix.get(y_matrix.size() - 1).size() - 1));
            }
            outfile.print("]");
        }
        outfile.print("]");
        outfile.println(" in module ReadTable");
        outfile.close();
        infile.close();
    }
}

