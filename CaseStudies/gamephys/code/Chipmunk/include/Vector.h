/* Copyright (c) 2013 Scott Lembcke and Howling Moon Software
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

 // VECTOR_H - VECTOR MODULE //

#ifndef VECTOR_H
#define VECTOR_H

#include <stdio.h>
#include <stdbool.h>
#include <float.h>
#include <math.h>

typedef struct Vector {
    double x;
    double y;
} Vector;

// Error value for unit testing.
#ifdef UNIT_TEST
    static const Vector errorVect = {INT_MAX, INT_MIN};
    #define VECT_ERR errorVect
#endif

// Zero vector constant.
static const Vector zeroVect = {0.0f, 0.0f};

// Constructor for vectors.
static inline Vector vect(const double x, const double y) {
    Vector newVect = {x, y};
    return newVect;
}

// Equality function. Compares doubles, so may be inaccurate. Use with caution.
static inline bool vectEqual(const Vector v1, const Vector v2) {
    return (v1.x == v2.x && v1.y == v2.y);
}

// Basic vector operations:
// Vector addition.
static inline Vector vectAdd(const Vector v1, const Vector v2) {
    return vect(v1.x + v2.x, v1.y + v2.y);
}

// Vector subtraction.
static inline Vector vectSub(const Vector v1, const Vector v2) {
    return vect(v1.x - v2.x, v1.y - v2.y);
}

// Vector scalar multiplication.
static inline Vector vectMult(const Vector v, double s) {
    return vect(s * v.x, s * v.y);
}

// Vector negation.
static inline Vector vectNeg(const Vector v) {
    return vect(-v.x, -v.y);
}

// Vector dot product.
static inline double vectDot(const Vector v1, const Vector v2) {
    return (v1.x * v2.x) + (v1.y * v2.y);
}

// Vector cross product.
static inline double vectCross(const Vector v1, const Vector v2) {
    return (v1.x * v2.y) - (v1.y * v2.x);
}

// Returns a perpendicular vector (90 degree rotation).
static inline Vector vectPerp(const Vector v) {
    return vect(-v.y, v.x);
}

// Returns a perpendicular vector (-90 degree rotation).
static inline Vector vectRPerp(const Vector v) {
    return vect(v.y, -v.x);
}

// Returns a projection of v1 onto v2.
static inline Vector vectProject(const Vector v1, const Vector v2) {
    return vectMult(v2, vectDot(v1, v2) / vectDot(v2, v2));
}

// Returns unit vector for the specified angle (in radians).
static inline Vector vectForAngle(const double rad) {
    return vect(cos(rad), sin(rad));
}

// Returns the angle of the specified vector.
static inline double vectToAngle(const Vector v) {
    return atan2(v.y, v.x);
}

// Rotates v1 by v2 (using complex multiplication).
static inline Vector vectRotate(const Vector v1, const Vector v2) {
    return vect(v1.x*v2.x - v1.y*v2.y, v1.x*v2.y + v1.y*v2.x);
}

// Inverse of vectRotate().
static inline Vector vectUnrotate(const Vector v1, const Vector v2) {
    return vect(v1.x*v2.x + v1.y*v2.y, v1.y*v2.x - v1.x*v2.y);
}

// Returns the squared length of v (|v|^2).
static inline double vectLengthSq(const Vector v) {
    return vectDot(v, v);
}

// Returns the length of v (|v|).
static inline double vectLength(const Vector v) {
    return sqrt(vectLengthSq(v));
}

// Normalizes a vector.
static inline Vector vectNormalize(const Vector v) {
    return vectMult(v, 1.0f / vectLength(v) + DBL_MIN);
}

// Clamps a vector to a specified length.
static inline Vector vectClamp(const Vector v, const double len) {
    return (vectLength(v) < len) ? v : vectMult(vectNormalize(v), len);
}

// Linearly interpolate between v1 and v2.
static inline Vector vectLerp(const Vector v1, const Vector v2, const double t)
{
    return vectAdd(vectMult(v1, 1.0f - t), vectMult(v2, t));
}

// Returns the squared distance between v1 and v2.
static inline double vectDistSq(const Vector v1, const Vector v2) {
    return vectLengthSq(vectSub(v1, v2));
}

// Returns the distance between v1 and v2.
static inline double vectDist(const Vector v1, const Vector v2) {
    return sqrt(vectDistSq(v1, v2));
}

// Checks if two vectors are within a certain distance dist of each other.
static inline bool vectNear(const Vector v1, const Vector v2, const double dist) {
    return vectDist(v1, v2) < dist;
}

#endif
