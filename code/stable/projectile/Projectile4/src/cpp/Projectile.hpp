#ifndef Projectile_h
#define Projectile_h

#define _USE_MATH_DEFINES

#include <math.h>
#include <string>

using std::ifstream;
using std::ofstream;
using std::string;

double func_t_flight(double v_launch, double theta, double g_vect);

double func_p_land(double v_launch, double theta, double g_vect);

double func_d_offset(double p_target, double p_land);

string func_s(double p_target, double epsilon, double d_offset);

void get_input(string filename, double &v_launch, double &theta, double &p_target);

void input_constraints(double v_launch, double theta, double p_target);

void write_output(string s, double d_offset);

#endif
