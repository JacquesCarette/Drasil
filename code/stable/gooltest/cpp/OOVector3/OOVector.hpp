#ifndef OOVector_h
#define OOVector_h

#include <string>
#include <vector>

using std::string;

/** \brief Vectors of doubles and common vector-related operations.
 */
class Vector {
    public:
        /** \brief Construct a vector from an array of doubles.
            \param v The doubles.
        */
        Vector(std::vector<double> v);
        /** \brief Returns the dimension of this vector.
            \return The dimension of the vector.
        */
        int dimension();
        /** \brief Calculate the Euclidean norm (magnitude) of this vector.
            \return The magnitude.
        */
        double magnitude();
        /** \brief Calculate unit vector of this vector.
            \return A new unit vector.
        */
        Vector norm();
        /** \brief Calculate the dot product of two vectors.
            \param v1 First vector.
            \param v2 Second vector.
            \return The dot product.
        */
        static double dot(Vector v1, Vector v2);
        /** \brief Calculate the resultant vector of two vectors.
            \param v1 First vector.
            \param v2 Second vector.
            \return The resultant vector.
        */
        static Vector add(Vector v1, Vector v2);
        /** \brief Scale this vector by a factor.
            \param s Scalar factor.
            \return A new scaled vector.
        */
        Vector scale(double s);
        /** \brief Prints the vector elements to console.
        */
        void print();
    
    private:
        std::vector<double> v;

};

#endif
