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

// SEGMENT_SHAPE_C - SEGMENT SHAPE MODULE //

#include "../include/Chipmunk.h"

SegmentShape *segmentShapeAlloc(void) {
    return (SegmentShape *)calloc(1, sizeof(SegmentShape));
}

static BB segmentShapeCacheData(SegmentShape *seg, Transform transform) {
    seg->ta = transformPoint(transform, seg->a);
    seg->tb = transformPoint(transform, seg->b);
    seg->tn = transformPoint(transform, seg->n);

    double left, right, bottom, top;

    if (seg->ta.x < seg->tb.x) {
        left = seg->ta.x;
        right = seg->tb.x;
    } else {
        left = seg->tb.x;
        right = seg->ta.x;
    }

    if (seg->ta.y < seg->tb.y) {
        top = seg->tb.y;
        bottom = seg->ta.y;
    } else {
        top = seg->ta.y;
        bottom = seg->tb.y;
    }

    double radius = seg->radius;
    return BBNew(left - radius, bottom - radius, right + radius, top + radius);
}

static ShapeMassInfo segmentShapeMassInfo(double mass, Vector a, Vector b, double radius) {
    ShapeMassInfo info = {
        mass,
        momentForBox(1.0f, vectDist(a, b) + 2.0f * radius, 2.0f * radius)
        /* approximation */,
        vectLerp(a, b, 0.5f),
        areaForSegment(a, b, radius),
    };

    return info;
}

static const ShapeClass SegmentShapeClass = {
    SEGMENT_SHAPE,
    (ShapeCacheDataImpl) segmentShapeCacheData,
    NULL,
};

SegmentShape *segmentShapeInit(SegmentShape *seg, Body *body, Vector a, Vector b, double radius) {
    seg->a = a;
    seg->b = b;
    seg->n = vectRPerp(vectNormalize(vectSub(b, a)));

    seg->radius = radius;

    seg->aTangent = zeroVect;
    seg->bTangent = zeroVect;

    shapeInit((Shape *) seg, &SegmentShapeClass, body, segmentShapeMassInfo(0.0f, a, b, radius));

    return seg;
}

Shape *segmentShapeNew(Body *body, Vector a, Vector b, double radius) {
    SegmentShape *newSeg = segmentShapeAlloc();
    return (Shape *) segmentShapeInit(newSeg, body, a, b, radius);
}

Vector segmentShapeGetA(const Shape *shape) {
    assertHard(shape->klass == &SegmentShapeClass, VECT_ERR, "Shape is not a segment shape.");
    return ((SegmentShape *) shape)->a;
}

Vector segmentShapeGetB(const Shape *shape) {
    assertHard(shape->klass == &SegmentShapeClass, VECT_ERR, "Shape is not a segment shape.");
    return ((SegmentShape *) shape)->b;
}

Vector segmentShapeGetNormal(const Shape *shape) {
    assertHard(shape->klass == &SegmentShapeClass, VECT_ERR, "Shape is not a segment shape.");
    return ((SegmentShape *) shape)->n;
}

double segmentShapeGetRadius(const Shape *shape) {
    assertHard(shape->klass == &SegmentShapeClass, DBL_ERR, "Shape is not a segment shape.");
    return ((SegmentShape *) shape)->radius;
}

// Setters (NOTE: the last two are tagged unsafe):

void segmentShapeSetNeighbors(Shape *shape, Vector prev, Vector next) {
    assertHard(shape->klass == &SegmentShapeClass, VOID_ERR, "Shape is not a segment shape.");
    SegmentShape *seg = (SegmentShape *) shape;

    seg->aTangent = vectSub(prev, seg->a);
    seg->bTangent = vectSub(next, seg->b);
}

void segmentShapeSetEndpoints(Shape *shape, const Vector a, const Vector b) {
    assertHard(shape->klass == &SegmentShapeClass, VOID_ERR, "Shape is not a segment shape.");

    SegmentShape *seg = (SegmentShape *) shape;

    seg->a = a;
    seg->b = b;
    seg->n = vectPerp(vectNormalize(vectSub(b, a)));

    double mass = shape->massInfo.mass;
    shape->massInfo = segmentShapeMassInfo(mass, seg->a, seg->b, seg->radius);
    if (mass > 0.0f) bodyAccumulateMassFromShapes(shape->body);
}

void segmentShapeSetRadius(Shape *shape, const double radius) {
    assertHard(shape->klass == &SegmentShapeClass, VOID_ERR, "Shape is not a segment shape.");

    SegmentShape *seg = (SegmentShape *) shape;

    seg->radius = radius;

    double mass = shape->massInfo.mass;
    shape->massInfo = segmentShapeMassInfo(mass, seg->a, seg->b, seg->radius);
    if (mass > 0.0f) bodyAccumulateMassFromShapes(shape->body);
}
