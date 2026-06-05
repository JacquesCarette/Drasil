import math

## \brief Vectors of doubles and common vector-related operations.
class Vector:
    ## \brief Construct a vector from an array of doubles.
    # \param v The doubles.
    def __init__(self, v):
        assert len(v) > 0, "Vector dimension must be > 0."
        self.v = v.copy()
    
    ## \brief Returns the dimension of this vector.
    # \return The dimension of the vector.
    def dimension(self):
        return len(self.v)
    
    ## \brief Calculate the Euclidean norm (magnitude) of this vector.
    # \return The magnitude.
    def magnitude(self):
        return math.sqrt(Vector.dot(self, self))
    
    ## \brief Calculate unit vector of this vector.
    # \return A new unit vector.
    def norm(self):
        mag = self.magnitude()
        assert mag > 0.0, "Cannot normalize a zero vector."
        return self.scale(1.0 / mag)
    
    ## \brief Calculate the dot product of two vectors.
    # \param v1 First vector.
    # \param v2 Second vector.
    # \return The dot product.
    @staticmethod
    def dot(v1, v2):
        assert v1.dimension() == v2.dimension(), "Vector dimensions must match for dot product."
        res = 0.0
        for i in range(0, v1.dimension(), 1):
            res += v1.v[i] * v2.v[i];
        return res
    
    ## \brief Calculate the resultant vector of two vectors.
    # \param v1 First vector.
    # \param v2 Second vector.
    # \return The resultant vector.
    @staticmethod
    def add(v1, v2):
        assert v1.dimension() == v2.dimension(), "Vector dimensions must match for addition."
        res = v1.v.copy()
        for i in range(0, v1.dimension(), 1):
            res[i] += v2.v[i];
        return Vector(res)
    
    ## \brief Scale this vector by a factor.
    # \param s Scalar factor.
    # \return A new scaled vector.
    def scale(self, s):
        res = self.v.copy()
        for i in range(0, self.dimension(), 1):
            res[i] = s * res[i]
        return Vector(res)
    
    ## \brief Prints the vector elements to console.
    def print(self):
        print(self.v)

ds1 = [1.0, 2.0, 3.0]
ds2 = [4.0, 5.0, 6.0]
v1 = Vector(ds1)
v2 = Vector(ds2)
print("v1: ", end="")
v1.print()
print("v2: ", end="")
v2.print()
d = Vector.dot(v1, v2)
print("Dot product: ", end="")
print(d)
m = v1.magnitude()
print("Magnitude of v1: ", end="")
print(m)
vAdd = Vector.add(v1, v2)
print("v1 + v2: ", end="")
vAdd.print()
vUnit = Vector.add(v1, v2.scale(2.0)).norm()
print("Unit vector of v1 + 2 * v2: ", end="")
vUnit.print()
