package Projectile;

/** \file Calculations.java
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
public class Calculations {
    
    /** \brief Calculates flight duration: the time when the projectile lands (s)
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param g_vect gravitational acceleration (m/s^2)
        \return flight duration: the time when the projectile lands (s)
    */
    public static double func_t_flight(double v_launch, double theta, double g_vect) {
        return 2 * v_launch * Math.sin(theta) / g_vect;
    }
    
    /** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param g_vect gravitational acceleration (m/s^2)
        \return landing position: the distance from the launcher to the final position of the projectile (m)
    */
    public static double func_p_land(double v_launch, double theta, double g_vect) {
        return 2 * Math.pow(v_launch, 2) * Math.sin(theta) * Math.cos(theta) / g_vect;
    }
    
    /** \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \param p_target target position: the distance from the launcher to the target (m)
        \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
        \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
    */
    public static double func_d_offset(double p_target, double p_land) {
        return p_land - p_target;
    }
    
    /** \brief Calculates output message as a string
        \param p_target target position: the distance from the launcher to the target (m)
        \param epsilon hit tolerance
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \return output message as a string
    */
    public static String func_s(double p_target, double epsilon, double d_offset) {
        if (Math.abs(d_offset / p_target) < epsilon) {
            return "The target was hit.";
        }
        else if (d_offset < 0) {
            return "The projectile fell short.";
        }
        else {
            return "The projectile went long.";
        }
    }
}
