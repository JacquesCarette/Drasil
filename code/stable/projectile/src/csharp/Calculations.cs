/** \file Calculations.cs
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class Calculations {
    
    /** \brief Calculates flight duration: the time when the projectile lands (s)
        \param inParams structure holding the input values
        \return flight duration: the time when the projectile lands (s)
    */
    public static double func_t_flight(InputParameters inParams) {
        return 2 * inParams.v_launch * Math.Sin(inParams.theta) / 9.8;
    }
    
    /** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
        \param inParams structure holding the input values
        \return landing position: the distance from the launcher to the final position of the projectile (m)
    */
    public static double func_p_land(InputParameters inParams) {
        return 2 * Math.Pow(inParams.v_launch, 2) * Math.Sin(inParams.theta) * Math.Cos(inParams.theta) / 9.8;
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
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \return output message as a string
    */
    public static string func_s(InputParameters inParams, double d_offset) {
        if (Math.Abs(d_offset / inParams.p_target) < 2.0e-2) {
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

