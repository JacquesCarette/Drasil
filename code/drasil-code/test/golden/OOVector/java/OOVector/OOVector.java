package OOVector;

import java.util.Arrays;

public class OOVector {
    
    public static void main(String[] args) {
        double[] ds1 = {1.0, 2.0, 3.0};
        double[] ds2 = {4.0, 5.0, 6.0};
        Vector v1 = new Vector(ds1);
        Vector v2 = new Vector(ds2);
        System.out.print("v1: ");
        v1.printSelf();
        System.out.print("v2: ");
        v2.printSelf();
        double d = Vector.dot(v1, v2);
        System.out.print("Dot product: ");
        System.out.println(d);
        double m = v1.magnitude();
        System.out.print("Magnitude of v1: ");
        System.out.println(m);
        Vector vAdd = Vector.add(v1, v2);
        System.out.print("v1 + v2: ");
        vAdd.printSelf();
        Vector vUnit = Vector.norm(Vector.add(v1, Vector.scale(v2, 2.0)));
        System.out.print("Unit vector of v1 + 2 * v2: ");
        vUnit.printSelf();
    }
}

/** \brief Vectors of doubles and common vector-related operations.
*/
class Vector {
    private double[] v;
    
    /** \brief Construct a vector from an array of doubles.
        \param v The doubles.
    */
    public Vector(double[] v) {
        assert v.length > 0 : "Vector dimension must be > 0.";
        this.v = v.clone();
    }
    
    /** \brief Returns the dimension of this vector.
        \return The dimension of the vector.
    */
    public int dimension() {
        return this.v.length;
    }
    
    /** \brief Calculate the Euclidean norm (magnitude) of this vector.
        \return The magnitude.
    */
    public double magnitude() {
        return Math.sqrt(Vector.dot(this, this));
    }
    
    /** \brief Calculate unit vector of this vector.
        \return A new unit vector.
    */
    public static Vector norm(Vector v) {
        double mag = v.magnitude();
        assert mag > 0.0 : "Cannot normalize a zero vector.";
        return Vector.scale(v, 1.0 / mag);
    }
    
    /** \brief Calculate the dot product of two vectors.
        \param v1 First vector.
        \param v2 Second vector.
        \return The dot product.
    */
    public static double dot(Vector v1, Vector v2) {
        assert v1.dimension() == v2.dimension() : "Vector dimensions must match for dot product.";
        double res = 0.0;
        for (int i = 0; i < v1.dimension(); i += 1) {
            res += v1.v[i] * v2.v[i];
        }
        return res;
    }
    
    /** \brief Calculate the resultant vector of two vectors.
        \param v1 First vector.
        \param v2 Second vector.
        \return The resultant vector.
    */
    public static Vector add(Vector v1, Vector v2) {
        assert v1.dimension() == v2.dimension() : "Vector dimensions must match for addition.";
        double[] res = v1.v.clone();
        for (int i = 0; i < v1.dimension(); i += 1) {
            res[i] += v2.v[i];
        }
        return new Vector(res);
    }
    
    /** \brief Scale this vector by a factor.
        \param v Scalar factor.
        \return A new scaled vector.
    */
    public static Vector scale(Vector v, double s) {
        double[] res = v.v.clone();
        for (int i = 0; i < v.dimension(); i += 1) {
            res[i] = s * res[i];
        }
        return new Vector(res);
    }
    
    /** \brief Prints the vector elements to console.
    */
    public void printSelf() {
        System.out.println(Arrays.toString(this.v));
    }
}
