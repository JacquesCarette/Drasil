package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class ReadTable {
    
    public static Vector<Double> read_z_array(String filename) throws Exception {
        Scanner infile;
        infile = new Scanner(new File(filename));
        String line;
        line = infile.nextLine();
        infile.close();
        Vector<String> z_array_str = new Vector<String>(0);
        z_array_str = new Vector<String>(Arrays.asList(line.split(",")));
        {   Vector<String> temp = new Vector<String>(0);
            for (int i_temp = 1; i_temp < z_array_str.size(); i_temp += 2) {
                temp.add(z_array_str.get(i_temp));
            }
            z_array_str = temp;
        }
        Vector<Double> z_array = new Vector<Double>(0);
        for (int i = 0; i < z_array_str.size(); i++) {
            z_array.add(Double.parseDouble(z_array_str.get(i)));
        }
        return z_array;
    }
    
    public static Vector< Vector<Double> > read_x_array(String filename) throws Exception {
        Scanner infile;
        infile = new Scanner(new File(filename));
        Vector<String> lines = new Vector<String>(0);
        while (infile.hasNextLine()) {
            lines.add(infile.nextLine());
        }
        infile.close();
        {   Vector<String> temp = new Vector<String>(0);
            for (int i_temp = 1; i_temp < lines.size(); i_temp++) {
                temp.add(lines.get(i_temp));
            }
            lines = temp;
        }
        Vector< Vector<String> > x_array_str = new Vector< Vector<String> >(0);
        for (int i = 0; i < lines.size(); i++) {
            Vector<String> temp_str = new Vector<String>(0);
            temp_str = new Vector<String>(Arrays.asList(lines.get(i).split(",")));
            {   Vector<String> temp = new Vector<String>(0);
                for (int i_temp = 0; i_temp < temp_str.size(); i_temp += 2) {
                    temp.add(temp_str.get(i_temp));
                }
                temp_str = temp;
            }
            x_array_str.add(temp_str);
        }
        Vector< Vector<Double> > x_array = new Vector< Vector<Double> >(0);
        for (int i = 0; i < x_array_str.size(); i++) {
            Vector<Double> nextLine = new Vector<Double>(0);
            for (int j = 0; j < x_array_str.get(i).size(); j++) {
                nextLine.add(Double.parseDouble(x_array_str.get(i).get(j)));
            }
            x_array.add(nextLine);
        }
        return x_array;
    }
    
    public static Vector< Vector<Double> > read_y_array(String filename) throws Exception {
        Scanner infile;
        infile = new Scanner(new File(filename));
        Vector<String> lines = new Vector<String>(0);
        while (infile.hasNextLine()) {
            lines.add(infile.nextLine());
        }
        infile.close();
        {   Vector<String> temp = new Vector<String>(0);
            for (int i_temp = 1; i_temp < lines.size(); i_temp++) {
                temp.add(lines.get(i_temp));
            }
            lines = temp;
        }
        Vector< Vector<String> > y_array_str = new Vector< Vector<String> >(0);
        for (int i = 0; i < lines.size(); i++) {
            Vector<String> temp_str = new Vector<String>(0);
            temp_str = new Vector<String>(Arrays.asList(lines.get(i).split(",")));
            {   Vector<String> temp = new Vector<String>(0);
                for (int i_temp = 1; i_temp < temp_str.size(); i_temp += 2) {
                    temp.add(temp_str.get(i_temp));
                }
                temp_str = temp;
            }
            y_array_str.add(temp_str);
        }
        Vector< Vector<Double> > y_array = new Vector< Vector<Double> >(0);
        for (int i = 0; i < y_array_str.size(); i++) {
            Vector<Double> nextLine = new Vector<Double>(0);
            for (int j = 0; j < y_array_str.get(i).size(); j++) {
                nextLine.add(Double.parseDouble(y_array_str.get(i).get(j)));
            }
            y_array.add(nextLine);
        }
        return y_array;
    }
}

