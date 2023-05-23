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

/** \brief Structure for holding the input values and constant values
*/
class InputParameters {
    public:
        float v_launch;
        float theta;
        float p_target;
        float g = 9.8f;
        float epsilon = 2.0e-2f;
        
        /** \brief Initializes input object by reading inputs and checking physical constraints on the input
            \param filename name of the input file
        */
        InputParameters(string filename);
    
    private:
        /** \brief Reads input from a file with the given file name
            \param filename name of the input file
        */
        void get_input(string filename);
        /** \brief Verifies that input values satisfy the physical constraints
        */
        void input_constraints();
};

/** \brief Calculates flight duration: the time when the projectile lands (s)
    \param inParams structure holding the input values
    \return flight duration: the time when the projectile lands (s)
*/
float func_t_flight(InputParameters &inParams);

/** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
    \param inParams structure holding the input values
    \return landing position: the distance from the launcher to the final position of the projectile (m)
*/
float func_p_land(InputParameters &inParams);

/** \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \param inParams structure holding the input values
    \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
    \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
*/
float func_d_offset(InputParameters &inParams, float p_land);

/** \brief Calculates output message as a string
    \param inParams structure holding the input values
    \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \return output message as a string
*/
string func_s(InputParameters &inParams, float d_offset);

/** \brief Writes the output values to output.txt
    \param s output message as a string
    \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \param t_flight flight duration: the time when the projectile lands (s)
*/
void write_output(string s, float d_offset, float t_flight);

#endif
