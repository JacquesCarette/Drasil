package DblPendulum;

/** \file ODE.java
    \author Dong Chen
    \brief Class representing an ODE system
*/
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations;

/** \brief Class representing an ODE system
*/
public class ODE implements FirstOrderDifferentialEquations {
    private double m_1;
    private double m_2;
    private double L_1;
    private double L_2;
    
    /** \brief Constructor for ODE objects
        \param m_1 mass of the first object (kg)
        \param m_2 mass of the second object (kg)
        \param L_1 length of the first rod (m)
        \param L_2 length of the second rod (m)
    */
    public ODE(double m_1, double m_2, double L_1, double L_2) {
        this.m_1 = m_1;
        this.m_2 = m_2;
        this.L_1 = L_1;
        this.L_2 = L_2;
    }
    
    /** \brief returns the ODE system dimension
        \return dimension of the ODE system
    */
    public int getDimension() {
        return 4;
    }
    
    /** \brief function representation of an ODE system
        \param t current independent variable value in ODE solution
        \param theta dependent variables (rad)
        \param dtheta change in dependent variables (rad)
    */
    public void computeDerivatives(double t, double[] theta, double[] dtheta) {
        dtheta[0] = theta[1];
        dtheta[1] = (-9.8 * (2.0 * m_1 + m_2) * Math.sin(theta[0]) - m_2 * 9.8 * Math.sin(theta[0] - 2.0 * theta[2]) - 2.0 * Math.sin(theta[0] - theta[2]) * m_2 * (Math.pow(theta[3], 2.0) * L_2 + Math.pow(theta[1], 2.0) * L_1 * Math.cos(theta[0] - theta[2]))) / (L_1 * (2.0 * m_1 + m_2 - m_2 * Math.cos(2.0 * theta[0] - 2.0 * theta[2])));
        dtheta[2] = theta[3];
        dtheta[3] = 2.0 * Math.sin(theta[0] - theta[2]) * (Math.pow(theta[1], 2.0) * L_1 * (m_1 + m_2) + 9.8 * (m_1 + m_2) * Math.cos(theta[0]) + Math.pow(theta[3], 2.0) * L_2 * m_2 * Math.cos(theta[0] - theta[2])) / (L_2 * (2.0 * m_1 + m_2 - m_2 * Math.cos(2.0 * theta[0] - 2.0 * theta[2])));
    }
}
