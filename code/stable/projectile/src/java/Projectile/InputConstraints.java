package Projectile;

/** \file InputConstraints.java
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
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
            System.out.print("Warning: ");
            System.out.print("v_launch has value ");
            System.out.print(inParams.v_launch);
            System.out.print(" but suggested to be ");
            System.out.print("above ");
            System.out.print(0);
            System.out.println(".");
        }
        if (!(0 < inParams.theta && inParams.theta < 3.14159265 / 2)) {
            System.out.print("Warning: ");
            System.out.print("theta has value ");
            System.out.print(inParams.theta);
            System.out.print(" but suggested to be ");
            System.out.print("between ");
            System.out.print(0);
            System.out.print(" and ");
            System.out.print(3.14159265 / 2);
            System.out.print(" ((pi)/(2))");
            System.out.println(".");
        }
        if (!(inParams.p_target > 0)) {
            System.out.print("Warning: ");
            System.out.print("p_target has value ");
            System.out.print(inParams.p_target);
            System.out.print(" but suggested to be ");
            System.out.print("above ");
            System.out.print(0);
            System.out.println(".");
        }
    }
}

