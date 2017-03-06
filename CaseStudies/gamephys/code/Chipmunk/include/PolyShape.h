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

// POLY_SHAPE_H - POLY SHAPE MODULE //

#ifndef POLY_SHAPE_H
#define POLY_SHAPE_H

// Inline functions:
// Returns the moment of inertia for a polygon.
static inline double momentForPoly(const double mass, const int count, const Vector *verts, const Vector offset, const double radius) {
    if (count == 2) return momentForSegment(mass, verts[0], verts[1], 0.0f);

    double sum1 = 0.0f;
    double sum2 = 0.0f;
    for (int i = 0; i < count; ++i) {
        Vector v1 = vectAdd(verts[i], offset);
        Vector v2 = vectAdd(verts[(i + 1) % count], offset);

        double a = vectCross(v2, v1);
        double b = vectDot(v1, v1) + vectDot(v1, v2) + vectDot(v2, v2);

        sum1 += a * b;
        sum2 += a;
    }

    return (mass * sum1) / (6.0f * sum2);
}

// Calculates the area of a polygon.
static inline double areaForPoly(const int count, const Vector *verts, const double radius) {
    double area = 0.0f;
    double perimeter = 0.0f;

    for (int i = 0; i < count; ++i) {
        Vector v1 = verts[i];
        Vector v2 = verts[(i + 1) % count];

        area += vectCross(v1, v2);
        perimeter += vectDist(v1, v2);
    }

    return radius * (M_PI * fabs(radius) + perimeter) + area / 2.0f;
}

// Returns the centroid of a polygon.
static inline Vector centroidForPoly(const int count, const Vector *verts) {
    double sum = 0.0f;
    Vector vsum = zeroVect;

    for (int i = 0; i < count; ++i) {
        Vector v1 = verts[i];
        Vector v2 = verts[(i + 1) % count];
        double cross = vectCross(v1, v2);

        sum += cross;
        vsum = vectAdd(vsum, vectMult(vectAdd(v1, v2), cross));
    }

    return vectMult(vsum, 1.0f / (3.0f * sum));
}

// Returns the moment for a box shape given its dimensions.
static inline double momentForBox(const double mass, const double width, const double height) {
    return mass * (width * width + height * height) / 12.0f;
}

// Returns the moment for a box shape given its bounding box.
static inline double momentForBox2(const double mass, const BB box) {
    double width = box.right - box.left;
    double height = box.top - box.bottom;
    Vector offset = vectMult(vect(box.left + box.right, box.bottom + box.top), 0.5f);

    // NOTE: NaN when offset is 0 and m is infinity
    return momentForBox(mass, width, height) + mass * vectLengthSq(offset);
}

// Constructors for polygons.
PolyShape *polyShapeAlloc(void);
PolyShape *polyShapeInit(PolyShape *poly, Body *body, const int count, const Vector *verts, const double radius, Transform transform);
PolyShape *polyShapeInitRaw(PolyShape *poly, Body *body, const int count, const Vector *verts, const double radius);
Shape *polyShapeNew(Body *body, const int count, const Vector *verts, const double radius, Transform transform);
Shape *polyShapeNewRaw(Body *body, const int count, const Vector *verts, const double radius);

PolyShape *boxShapeInit(PolyShape *poly, Body *body, const double radius, const double width, const double height);
PolyShape *boxShapeInit2(PolyShape *poly, Body *body, const double radius, const BB box);

// Getters for polygons.
int polyShapeGetCount(const Shape *shape);
Vector polyShapeGetVert(const Shape *shape, const int index);
double polyShapeGetRadius(const Shape *shape);

// Setters for polygons (NOTE: tagged unsafe in the original program).
void polyShapeSetVerts(Shape *shape, const int count, Vector *verts, Transform transform);
void polyShapeSetVertsRaw(Shape *shape, const int count, Vector *verts);
void polyShapeSetRadius(Shape *shape, const double radius);

#endif
