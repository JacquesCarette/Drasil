package Projectile;

/** \file Calculations.java
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
public class Calculations {
    
    /** \brief Calculates flight duration: the time when the projectile lands (s)
        \param inParams structure holding the input values
        \param g magnitude of gravitational acceleration (m/s^2)
        \return flight duration: the time when the projectile lands (s)
    */
    public static double func_t_flight(InputParameters inParams, double g) {
        return 2.0 * inParams.v_launch * Math.sin(inParams.theta) / g;
    }
    
    /** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
        \param inParams structure holding the input values
        \param g magnitude of gravitational acceleration (m/s^2)
        \return landing position: the distance from the launcher to the final position of the projectile (m)
    */
    public static double func_p_land(InputParameters inParams, double g) {
        return 2.0 * Math.pow(inParams.v_launch, 2.0) * Math.sin(inParams.theta) * Math.cos(inParams.theta) / g;
    }
    
    /** \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \param inParams structure holding the input values
        \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
        \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
    */
    public static double func_d_offset(InputParameters inParams, double p_land) {
        return p_land - inParams.p_target;
    }
    
    /** \brief Calculates output message as a string
        \param inParams structure holding the input values
        \param epsilon hit tolerance
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \return output message as a string
    */
    public static String func_s(InputParameters inParams, double epsilon, double d_offset) {
        if (Math.abs(d_offset / inParams.p_target) < epsilon) {
            return "The target was hit.";
        }
        else if (d_offset < 0.0) {
            return "The projectile fell short.";
        }
        else {
            return "The projectile went long.";
        }
    }
}
