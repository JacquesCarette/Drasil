package Projectile;

/** \file InputFormat.java
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for reading inputs
*/
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class InputFormat {
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
        \return array containing the following values:
        \return launch speed: the initial speed of the projectile when launched (m/s)
        \return launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \return target position: the distance from the launcher to the target (m)
    */
    public static Object[] get_input(String filename) throws FileNotFoundException {
        float v_launch;
        float theta;
        float p_target;
        
        Scanner infile;
        infile = new Scanner(new File(filename));
        infile.nextLine();
        v_launch = Float.parseFloat(infile.nextLine());
        infile.nextLine();
        theta = Float.parseFloat(infile.nextLine());
        infile.nextLine();
        p_target = Float.parseFloat(infile.nextLine());
        infile.close();
        
        Object[] outputs = new Object[3];
        outputs[0] = v_launch;
        outputs[1] = theta;
        outputs[2] = p_target;
        return outputs;
    }
}
