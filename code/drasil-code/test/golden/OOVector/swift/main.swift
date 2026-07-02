import Foundation

/** Vectors of doubles and common vector-related operations.
*/
class Vector {
    private var v: [Double] = []
    
    /** Construct a vector from an array of doubles.
        - Parameter v: The doubles.
    */
    init(_ v: [Double]) {
        assert( v.count > 0 , "Vector dimension must be > 0.")
        self.v = v
    }
    
    /** Returns the dimension of this vector.
        - Returns: The dimension of the vector.
    */
    func dimension() -> Int {
        return self.v.count
    }
    
    /** Calculate the Euclidean norm (magnitude) of this vector.
        - Returns: The magnitude.
    */
    func magnitude() -> Double {
        return sqrt(Vector.dot(self, self))
    }
    
    /** Calculate unit vector of this vector.
        - Returns: A new unit vector.
    */
    static func norm(_ v: Vector) -> Vector {
        var mag: Double = v.magnitude()
        assert( mag > 0.0 , "Cannot normalize a zero vector.")
        return Vector.scale(v, 1.0 / mag)
    }
    
    /** Calculate the dot product of two vectors.
        - Parameter v1: First vector.
        - Parameter v2: Second vector.
        - Returns: The dot product.
    */
    static func dot(_ v1: Vector, _ v2: Vector) -> Double {
        assert( v1.dimension() == v2.dimension() , "Vector dimensions must match for dot product.")
        var res: Double = 0.0
        for i in [Int](stride(from: 0, to: v1.dimension(), by: 1)) {
            res += v1.v[i] * v2.v[i]
        }
        return res
    }
    
    /** Calculate the resultant vector of two vectors.
        - Parameter v1: First vector.
        - Parameter v2: Second vector.
        - Returns: The resultant vector.
    */
    static func add(_ v1: Vector, _ v2: Vector) -> Vector {
        assert( v1.dimension() == v2.dimension() , "Vector dimensions must match for addition.")
        var res: [Double] = v1.v
        for i in [Int](stride(from: 0, to: v1.dimension(), by: 1)) {
            res[i] += v2.v[i]
        }
        return Vector(res)
    }
    
    /** Scale this vector by a factor.
        - Parameter v: Scalar factor.
        - Returns: A new scaled vector.
    */
    static func scale(_ v: Vector, _ s: Double) -> Vector {
        var res: [Double] = v.v
        for i in [Int](stride(from: 0, to: v.dimension(), by: 1)) {
            res[i] = s * res[i]
        }
        return Vector(res)
    }
    
    /** Prints the vector elements to console.
    */
    func printSelf() -> Void {
        print(self.v)
    }
}

var ds1: [Double] = [1.0, 2.0, 3.0]
var ds2: [Double] = [4.0, 5.0, 6.0]
var v1: Vector = Vector(ds1)
var v2: Vector = Vector(ds2)
print("v1: ", terminator: "")
v1.printSelf()
print("v2: ", terminator: "")
v2.printSelf()
var d: Double = Vector.dot(v1, v2)
print("Dot product: ", terminator: "")
print(d)
var m: Double = v1.magnitude()
print("Magnitude of v1: ", terminator: "")
print(m)
var vAdd: Vector = Vector.add(v1, v2)
print("v1 + v2: ", terminator: "")
vAdd.printSelf()
var vUnit: Vector = Vector.norm(Vector.add(v1, Vector.scale(v2, 2.0)))
print("Unit vector of v1 + 2 * v2: ", terminator: "")
vUnit.printSelf()
