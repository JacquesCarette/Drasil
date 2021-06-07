package GlassBR;

/** \file ReadTable.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides a function for reading glass ASTM data
*/
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

public class ReadTable {
    
    /** \brief Reads glass ASTM data from a file with the given file name
        \param filename name of the input file
        \param z_vector list of z values
        \param x_matrix lists of x values at different z values
        \param y_matrix lists of y values at different z values
    */
    public static void read_table(String filename, ArrayList<Double> z_vector, ArrayList<ArrayList<Double>> x_matrix, ArrayList<ArrayList<Double>> y_matrix) throws FileNotFoundException, IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function read_table called with inputs: {");
        outfile.print("  filename = ");
        outfile.print(filename);
        outfile.println(", ");
        outfile.print("  z_vector = ");
        outfile.print(z_vector);
        outfile.println(", ");
        outfile.print("  x_matrix = ");
        outfile.print(x_matrix);
        outfile.println(", ");
        outfile.print("  y_matrix = ");
        outfile.println(y_matrix);
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
        outfile.print("var 'z_vector' assigned ");
        outfile.print(z_vector);
        outfile.println(" in module ReadTable");
        outfile.close();
        while (infile.hasNextLine()) {
            lines.add(infile.nextLine());
        }
        for (int i = 0; i < lines.size(); i += 1) {
            linetokens = new ArrayList<String>(Arrays.asList(lines.get(i).split(",")));
            ArrayList<Double> x_matrix_temp = new ArrayList<Double>();
            ArrayList<Double> y_matrix_temp = new ArrayList<Double>();
            for (int stringlist_i = 0; stringlist_i < linetokens.size() / 2; stringlist_i += 1) {
                x_matrix_temp.add(Double.parseDouble(linetokens.get(stringlist_i * 2 + 0)));
                y_matrix_temp.add(Double.parseDouble(linetokens.get(stringlist_i * 2 + 1)));
            }
            x_matrix.add(x_matrix_temp);
            y_matrix.add(y_matrix_temp);
        }
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'x_matrix' assigned ");
        outfile.print(x_matrix);
        outfile.println(" in module ReadTable");
        outfile.close();
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'y_matrix' assigned ");
        outfile.print(y_matrix);
        outfile.println(" in module ReadTable");
        outfile.close();
        infile.close();
    }
}
