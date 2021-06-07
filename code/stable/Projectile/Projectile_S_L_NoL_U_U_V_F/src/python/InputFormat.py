## \file InputFormat.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Provides the function for reading inputs
## \brief Reads input from a file with the given file name
# \param filename name of the input file
# \return launch speed: the initial speed of the projectile when launched (m/s)
# \return launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
# \return target position: the distance from the launcher to the target (m)
def get_input(filename):
    infile = open(filename, "r")
    infile.readline()
    v_launch = float(infile.readline())
    infile.readline()
    theta = float(infile.readline())
    infile.readline()
    p_target = float(infile.readline())
    infile.close()
    
    return v_launch, theta, p_target
