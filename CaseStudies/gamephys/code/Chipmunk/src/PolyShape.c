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

 // POLY_SHAPE_C - POLY SHAPE MODULE //

#include <string.h> // for memcpy in the convex hull functions
#include <math.h>

#include "../include/Chipmunk.h"

PolyShape *polyShapeAlloc(void) {
    return (PolyShape *) calloc(1, sizeof(PolyShape));
}

static void polyShapeDestroy(PolyShape *poly) {
    if (poly->count > POLY_SHAPE_INLINE_ALLOC) {
        free(poly->planes);
    }
}

static BB polyShapeCacheData(PolyShape *poly, Transform transform) {
    int count = poly->count;
    SplittingPlane *dest = poly->planes;
    SplittingPlane *src = dest + count;

    double left = (double) INFINITY;
    double right = -(double) INFINITY;
    double bottom = (double) INFINITY;
    double top = -(double) INFINITY;

    for (int i = 0; i < count; ++i) {
        Vector v = transformPoint(transform, src[i].v0);
        Vector n = transformVect(transform, src[i].n);

        dest[i].v0 = v;
        dest[i].n = n;

        left = fmin(left, v.x);
        right = fmax(right, v.x);
        bottom = fmin(bottom, v.y);
        top = fmax(top, v.y);
    }

    double radius = poly->radius;
    poly->shape.bb = BBNew(left - radius, bottom - radius, right + radius, top + radius);
    return poly->shape.bb;
}

static void setVerts(PolyShape *poly, const int count, const Vector *verts) {
    poly->count = count;

    if (count <= POLY_SHAPE_INLINE_ALLOC) {
        poly->planes = poly->_planes;
    } else {
        poly->planes = (SplittingPlane *) calloc(2 * count, sizeof(SplittingPlane));
    }

    for (int i = 0; i < count; ++i) {
        Vector a = verts[(i - 1 + count) % count];
        Vector b = verts[i];
        Vector n = vectNormalize(vectRPerp(vectSub(b, a)));

        poly->planes[i + count].v0 = b;
        poly->planes[i + count].n = n;
    }
}

static ShapeMassInfo polyShapeMassInfo(const double mass, const int count, const Vector *verts, const double radius) {
    // NOTE: moment is approximate due to radius.

    Vector centroid = centroidForPoly(count, verts);
    ShapeMassInfo info = {
        mass,
        momentForPoly(1.0f, count, verts, vectNeg(centroid), radius),
        centroid,
        areaForPoly(count, verts, radius)
    };

    return info;
}

static const ShapeClass PolyClass = {
    POLY_SHAPE,
    (ShapeCacheDataImpl) polyShapeCacheData,
    (ShapeDestroyImpl) polyShapeDestroy,
};

PolyShape *polyShapeInit(PolyShape *poly, Body *body, const int count, const Vector *verts, const double radius, Transform transform) {
    Vector *hullVerts = (Vector *) alloca(count * sizeof(Vector));

    // Transform verts before building hull in case of a negative scale.
    for (int i = 0; i < count; ++i) {
        hullVerts[i] = transformPoint(transform, verts[i]);
    }

    unsigned int hullCount = convexHull(count, hullVerts, hullVerts, NULL, 0.0);

    return polyShapeInitRaw(poly, body, hullCount, hullVerts, radius);
}

PolyShape *polyShapeInitRaw(PolyShape *poly, Body *body, const int count, const Vector *verts, const double radius) {
    shapeInit((Shape *) poly, &PolyClass, body, polyShapeMassInfo(0.0f, count, verts, radius));

    setVerts(poly, count, verts);
    poly->radius = radius;

    return poly;
}

Shape *polyShapeNew(Body *body, const int count, const Vector *verts, const double radius, Transform transform) {
    PolyShape *newPoly = polyShapeAlloc();

    return (Shape *) polyShapeInit(newPoly, body, count, verts, radius, transform);
}

Shape *polyShapeNewRaw(Body *body, const int count, const Vector *verts, const double radius) {
    PolyShape *newPoly = polyShapeAlloc();

    return (Shape *) polyShapeInitRaw(newPoly, body, count, verts, radius);
}

PolyShape *boxShapeInit(PolyShape *poly, Body *body, const double radius, const double width, const double height) {
    double hwidth = width / 2.0f;
    double hheight = height / 2.0f;

    return boxShapeInit2(poly, body, radius, BBNew(-hwidth, -hheight, hwidth, hheight));
}

PolyShape *boxShapeInit2(PolyShape *poly, Body *body, const double radius, const BB box) {
    Vector verts[] = {
        vect(box.right, box.bottom),
        vect(box.right, box.top),
        vect(box.left, box.top),
        vect(box.left, box.bottom),
    };

    return polyShapeInitRaw(poly, body, 4, verts, radius);
}

Shape *boxShapeNew(Body *body, const double radius, const double width, const double height) {
    PolyShape *newPoly = polyShapeAlloc();

    return (Shape *) boxShapeInit(newPoly, body, radius, width, height);
}

Shape *boxShapeNew2(Body *body, const double radius, const BB box) {
    PolyShape *newPoly = polyShapeAlloc();

    return (Shape *) boxShapeInit2(newPoly, body, radius, box);
}

int polyShapeGetCount(const Shape *shape) {
    assertHard(shape->klass == &PolyClass, INT_ERR, "Shape is not a poly shape.");
    return ((PolyShape *) shape)->count;
}

Vector polyShapeGetVert(const Shape *shape, const int index) {
    assertHard(shape->klass == &PolyClass, VECT_ERR, "Shape is not a poly shape.");

    int count = polyShapeGetCount(shape);
    assertHard(0 <= index && index < count, VECT_ERR, "Index out of range.");

    return ((PolyShape *) shape)->planes[index + count].v0;
}

double polyShapeGetRadius(const Shape *shape) {
    assertHard(shape->klass == &PolyClass, DBL_ERR, "Shape is not a poly shape.");
    return ((PolyShape *) shape)->radius;
}

// Setters (NOTE: tagged unsafe by the original Chipmunk):

void polyShapeSetVerts(Shape *shape, const int count, Vector *verts, Transform transform) {
    Vector *hullVerts = (Vector *) alloca(count * sizeof(Vector));

    // Transform verts before building the hull in case of a negative scale.
    for (int i = 0; i < count; ++i) {
        hullVerts[i] = transformPoint(transform, verts[i]);
    }

    unsigned int hullCount = convexHull(count, hullVerts, hullVerts, NULL, 0.0);
    polyShapeSetVertsRaw(shape, hullCount, hullVerts);
}

void polyShapeSetVertsRaw(Shape *shape, const int count, Vector *verts) {
    assertHard(shape->klass == &PolyClass, VOID_ERR, "Shape is not a poly shape.");

    PolyShape *poly = (PolyShape *) shape;
    polyShapeDestroy(poly);

    setVerts(poly, count, verts);

    double mass = shape->massInfo.mass;
    shape->massInfo = polyShapeMassInfo(shape->massInfo.mass, count, verts, poly->radius);
    if (mass > 0.0f) bodyAccumulateMassFromShapes(shape->body);
}

void polyShapeSetRadius(Shape *shape, const double radius) {
    assertHard(shape->klass == &PolyClass, VOID_ERR, "Shape is not a poly shape.");

    PolyShape *poly = (PolyShape *) shape;
    poly->radius = radius;
}
