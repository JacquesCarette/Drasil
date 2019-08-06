package Projectile;

/** \file InputConstraints.java
    \brief Provides the function for checking the physical constraints and software constraints on the input
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class InputConstraints {
    
    /** \brief Verifies that input values satisfy the physical constraints and software constraints
        \param inParams structure holding the input values
    */
    public static void input_constraints(InputParameters inParams) throws Exception {
        if (!(inParams.v_launch > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(0 < inParams.theta && inParams.theta < 3.14159265 / 2)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(inParams.p_target > 0)) {
            System.out.println("Warning: constraint violated");
        }
    }
}

