/** \file Calculations.cs
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
using System;

public class Calculations {
    
    /** \brief Calculates flight duration: the time when the projectile lands (s)
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param g magnitude of gravitational acceleration (m/s^2)
        \return flight duration: the time when the projectile lands (s)
    */
    public static float func_t_flight(float v_launch, float theta, float g) {
        return 2.0f * v_launch * (float)(Math.Sin(theta)) / g;
    }
    
    /** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param g magnitude of gravitational acceleration (m/s^2)
        \return landing position: the distance from the launcher to the final position of the projectile (m)
    */
    public static float func_p_land(float v_launch, float theta, float g) {
        return 2.0f * (float)(Math.Pow(v_launch, 2.0f)) * (float)(Math.Sin(theta)) * (float)(Math.Cos(theta)) / g;
    }
    
    /** \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \param p_target target position: the distance from the launcher to the target (m)
        \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
        \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
    */
    public static float func_d_offset(float p_target, float p_land) {
        return p_land - p_target;
    }
    
    /** \brief Calculates output message as a string
        \param p_target target position: the distance from the launcher to the target (m)
        \param epsilon hit tolerance
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \return output message as a string
    */
    public static string func_s(float p_target, float epsilon, float d_offset) {
        if (Math.Abs(d_offset / p_target) < epsilon) {
            return "The target was hit.";
        }
        else if (d_offset < 0.0f) {
            return "The projectile fell short.";
        }
        else {
            return "The projectile went long.";
        }
    }
}
