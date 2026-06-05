using System;
using System.Diagnostics;

public class OOVector {
    
    public static void Main(string[] args) {
        double[] ds1 = {1.0, 2.0, 3.0};
        double[] ds2 = {4.0, 5.0, 6.0};
        Vector v1 = new Vector(ds1);
        Vector v2 = new Vector(ds2);
        Console.Write("v1: ");
        v1.print();
        Console.Write("v2: ");
        v2.print();
        double d = Vector.dot(v1, v2);
        Console.Write("Dot product: ");
        Console.WriteLine(d);
        double m = v1.magnitude();
        Console.Write("Magnitude of v1: ");
        Console.WriteLine(m);
        Vector vAdd = Vector.add(v1, v2);
        Console.Write("v1 + v2: ");
        vAdd.print();
        Vector vUnit = Vector.add(v1, v2.scale(2.0)).norm();
        Console.Write("Unit vector of v1 + 2 * v2: ");
        vUnit.print();
    }
}

/** \brief Vectors of doubles and common vector-related operations.
*/
public class Vector {
    private double[] v;
    
    /** \brief Construct a vector from an array of doubles.
        \param v The doubles.
    */
    public Vector(double[] v) {
        Debug.Assert( v.Length > 0 , "Vector dimension must be > 0.");
        this.v = (double[])(v.Clone());
    }
    
    /** \brief Returns the dimension of this vector.
        \return The dimension of the vector.
    */
    public int dimension() {
        return this.v.Length;
    }
    
    /** \brief Calculate the Euclidean norm (magnitude) of this vector.
        \return The magnitude.
    */
    public double magnitude() {
        return Math.Sqrt(Vector.dot(this, this));
    }
    
    /** \brief Calculate unit vector of this vector.
        \return A new unit vector.
    */
    public Vector norm() {
        double mag = this.magnitude();
        Debug.Assert( mag > 0.0 , "Cannot normalize a zero vector.");
        return this.scale(1.0 / mag);
    }
    
    /** \brief Calculate the dot product of two vectors.
        \param v1 First vector.
        \param v2 Second vector.
        \return The dot product.
    */
    public static double dot(Vector v1, Vector v2) {
        Debug.Assert( v1.dimension() == v2.dimension() , "Vector dimensions must match for dot product.");
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
        Debug.Assert( v1.dimension() == v2.dimension() , "Vector dimensions must match for addition.");
        double[] res = (double[])(v1.v.Clone());
        for (int i = 0; i < v1.dimension(); i += 1) {
            res[i] += v2.v[i];
        }
        return new Vector(res);
    }
    
    /** \brief Scale this vector by a factor.
        \param s Scalar factor.
        \return A new scaled vector.
    */
    public Vector scale(double s) {
        double[] res = (double[])(this.v.Clone());
        for (int i = 0; i < this.dimension(); i += 1) {
            res[i] = s * res[i];
        }
        return new Vector(res);
    }
    
    /** \brief Prints the vector elements to console.
    */
    public void print() {
        Console.Write("[");
        Console.Write(string.Join(", ", this.v));
        Console.WriteLine("]");
    }
}
