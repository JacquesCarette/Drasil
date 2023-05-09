/** \file Projectile.hpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Contains the entire Projectile program
*/
#ifndef Projectile_h
#define Projectile_h

#define _USE_MATH_DEFINES

#include <math.h>
#include <string>

using std::ifstream;
using std::ofstream;
using std::string;

/** \brief Calculates flight duration: the time when the projectile lands (s)
    \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
    \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    \param g_vect gravitational acceleration (m/s^2)
    \return flight duration: the time when the projectile lands (s)
*/
double func_t_flight(double v_launch, double theta, double g_vect);

/** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
    \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
    \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    \param g_vect gravitational acceleration (m/s^2)
    \return landing position: the distance from the launcher to the final position of the projectile (m)
*/
double func_p_land(double v_launch, double theta, double g_vect);

/** \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \param p_target target position: the distance from the launcher to the target (m)
    \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
    \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
*/
double func_d_offset(double p_target, double p_land);

/** \brief Calculates output message as a string
    \param p_target target position: the distance from the launcher to the target (m)
    \param epsilon hit tolerance
    \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \return output message as a string
*/
string func_s(double p_target, double epsilon, double d_offset);

/** \brief Reads input from a file with the given file name
    \param filename name of the input file
    \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
    \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    \param p_target target position: the distance from the launcher to the target (m)
*/
void get_input(string filename, double &v_launch, double &theta, double &p_target);

/** \brief Verifies that input values satisfy the physical constraints
    \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
    \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    \param p_target target position: the distance from the launcher to the target (m)
*/
void input_constraints(double v_launch, double theta, double p_target);

/** \brief Writes the output values to output.txt
    \param s output message as a string
    \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \param t_flight flight duration: the time when the projectile lands (s)
*/
void write_output(string s, double d_offset, double t_flight);

#endif
