package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class Control {
    
    public static void main(String[] args) throws Exception {
        String filename = args[0];
        InputParameters inparams = new InputParameters();
        InputFormat.get_input(filename, inparams);
        DerivedValues.derived_params(inparams);
        InputConstraints.check_constraints(inparams);
        Vector<Double> w_array = ReadTable.read_z_array("TSD.txt");
        Vector< Vector<Double> > data_sd = ReadTable.read_x_array("TSD.txt");
        Vector< Vector<Double> > data_q = ReadTable.read_y_array("TSD.txt");
        Vector<Double> j_array = ReadTable.read_z_array("SDF.txt");
        Vector< Vector<Double> > data_asprat = ReadTable.read_x_array("SDF.txt");
        Vector< Vector<Double> > data_qstar = ReadTable.read_y_array("SDF.txt");
        double q = Interpolation.interpY(data_sd, data_q, w_array, inparams.sd, inparams.wtnt);
        double q_hat = Calculations.calc_q_hat(q, inparams);
        double j_tol = Calculations.calc_j_tol(inparams);
        double j = Interpolation.interpZ(data_asprat, data_qstar, j_array, inparams.asprat, q_hat);
        double q_hat_tol = Interpolation.interpY(data_asprat, data_qstar, j_array, inparams.asprat, j_tol);
        double pb = Calculations.calc_pb(j, inparams);
        double nfl = Calculations.calc_nfl(q_hat_tol, inparams);
        double lr = Calculations.calc_lr(nfl, inparams);
        Boolean is_safe1 = Calculations.calc_is_safe1(pb, inparams);
        Boolean is_safe2 = Calculations.calc_is_safe2(lr, q);
        OutputFormat.display_output("outputfile.txt", q, j, q_hat_tol, pb, lr, nfl, is_safe1, is_safe2, inparams);
        System.out.println("Main has been executed and the results have been written to 'outputfile.txt'.");
    }
}

