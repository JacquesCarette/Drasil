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

 // TRANSFORM_H - TRANSFORM MATRIX MODULE //

#ifndef TRANSFORM_H
#define TRANSFORM_H

#include <math.h>
#include "../include/Chipmunk.h"
#include "Vector.h"
#include "BB.h"

// Transform matrix structure definition.

typedef struct Transform {
    double a; double b;
    double c; double d;
    double tx; double ty;
} Transform;

// Identity transformation constant.
static const Transform identity = {1.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f};

// Constructs a new transform matrix, where (a, b) is the x-basis vector,
// (c, d) is the y-basis vector and (tx, ty) is the translation.
static inline Transform
transformNew(const double a, const double b, const double c,
    const double d, const double tx, const double ty) {
        Transform newT = {a, b, c, d, tx, ty};
        return newT;
    }

// Constructs a new transform matrix in transposed order.
static inline Transform
transformNewTranspose(const double a, const double c, const double tx,
    const double b, const double d, const double ty) {
        return transformNew(a, b, c, d, tx, ty);
    }

// Returns the inverse of a transform matrix.
static inline Transform
transformInverse(Transform t) {
    double detInv = 1.0 / (t.a * t.d - t.b * t.c);
    return transformNewTranspose(
        t.d * detInv, -t.c * detInv, (t.c * t.ty - t.tx * t.d) * detInv,
        -t.b * detInv, t.a * detInv, (t.tx * t.b - t.a * t.ty) * detInv
    );
}

// Multiplies two transform matrices.
static inline Transform
transformMult(Transform t1, Transform t2) {
    return transformNewTranspose(
        t1.a * t2.a + t1.c * t2.b, t1.a * t2.c + t1.c * t2.d,
        t1.a * t2.tx + t1.c * t2.ty + t1.tx,
        t1.b * t2.a + t1.d * t2.b, t1.b * t2.c + t1.d * t2.d,
        t1.b * t2.tx + t1.d * t2.ty + t1.ty
    );
}

// Transforms an absolute point (i.e. a vertex).
static inline Vector
transformPoint(Transform t, Vector p) {
    return vect(
        t.a * p.x + t.c * p.y + t.tx,
        t.b * p.x + t.d * p.y + t.ty
    );
}

// Transforms a vector (i.e. a normal).
static inline Vector
transformVect(Transform t, Vector v) {
    return vect(t.a * v.x + t.c * v.y, t.b * v.x + t.d * v.y);
}

// Transforms a bounding box.
static inline BB
transformBB(Transform t, BB bb) {
    Vector center = BBCenter(bb);
    double hwidth = (bb.right - bb.left) * 0.5;
    double hheight = (bb.top - bb.bottom) * 0.5;

    double a = t.a * hwidth;
    double b = t.c * hheight;
    double d = t.b * hwidth;
    double e = t.d * hheight;

    double hwidthfmax = fmax(fabs(a + b), fabs(a - b));
    double hheightfmax = fmax(fabs(d + e), fabs(d - e));

    return BBNewForExtents(transformPoint(t, center), hwidthfmax,
        hheightfmax);
}

// Creates a translation matrix.
static inline Transform transformTranslate(Vector translate) {
    return transformNewTranspose(
        1.0, 0.0, translate.x,
        0.0, 1.0, translate.y
    );
}

// Creates a scaling matrix.
static inline Transform transformScale(double scaleX, double scaleY) {
    return transformNewTranspose(
        scaleX, 0.0, 0.0,
        0.0, scaleY, 0.0
    );
}

// Creates a rotation matrix.
static inline Transform transformRotate(double radians) {
    Vector rot = vectForAngle(radians);
    return transformNewTranspose(
        rot.x, -rot.y, 0.0,
        rot.y, -rot.x, 0.0
    );
}

// Create a rigid transformation matrix (with translation and rotation).
static inline Transform transformRigid(Vector translate, double radians) {
    Vector rot = vectForAngle(radians);
    return transformNewTranspose(
        rot.x, -rot.y, translate.x,
        rot.y, -rot.x, translate.y
    );
}

// Returns the inverse of a rigid transformation matrix.
static inline Transform transformRigidInverse(Transform t) {
    return transformNewTranspose(
        t.d, -t.c, (t.c * t.ty - t.tx * t.d),
        -t.b, t.a, (t.tx * t.b - t.a * t.ty)
    );
}

/* Misc (but potentially useful transformation matrices)
// NOTE: Commented out for now since these are not specified in the MIS.

static inline Transform
transformWrap(Transform outer, Transform inner) {
    return transformMult(
        transformInverse(outer),
        transformMult(inner, outer)
    );
}

static inline Transform
transformWrapInverse(Transform outer, Transform inner) {
    return transformMult(
        outer,
        transformMult(inner, transformInverse(outer))
    );
}

static inline Transform transformOrtho(BB bb) {
    return transformNewTranspose(
        2.0 / (bb.right - bb.left), 0.0,
        -(bb.right + bb.left) / (bb.right - bb.left),
        0.0, 2.0 / (bb.top - bb.bottom),
        -(bb.top + bb.bottom) / (bb.top - bb.bottom)
    );
}

static inline Transform transformBoneScale(Vector v0, Vector v1) {
    Vector diff = vectSub(v1, v0);
    return transformNewTranspose(
        diff.x, -diff.y, v0.x,
        diff.y, diff.x, v0.y
    );
}

static inline Transform
transformAxialScale(Vector axis, Vector pivot, double scale) {
    double A = axis.x * axis.y * (scale - 1.0);
    double B = vectDot(axis, pivot) * (1.0 - scale);

    return transformNewTranspose(
        scale * axis.x * axis.x + axis.y * axis.y, A, axis.x * B,
        A, axis.x * axis.x + scale * axis.y * axis.y, axis.y * B
    );
}

*/

#endif
