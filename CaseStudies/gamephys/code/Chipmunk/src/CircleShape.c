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

// CIRCLE_SHAPE_C - CIRCLE SHAPE MODULE //

#include "../include/Chipmunk.h"

CircleShape *circleShapeAlloc(void) {
    return (CircleShape *) calloc(1, sizeof(CircleShape));
}

static BB circleShapeCacheData(CircleShape *circle, Transform transform) {
    Vector center = transformPoint(transform, circle->center);
    circle->tcenter = center;
    return BBNewForCircle(center, circle->radius);
}

static ShapeMassInfo circleShapeMassInfo(const double mass, const double radius, const Vector center) {
    ShapeMassInfo info = {
        mass,
        momentForCircle(1.0f, 0.0f, radius, zeroVect),
        center,
        areaForCircle(0.0f, radius),
    };

    return info;
}

static const ShapeClass CircleShapeClass = {
    CIRCLE_SHAPE,
    (ShapeCacheDataImpl) circleShapeCacheData,
    NULL,
};

CircleShape *circleShapeInit(CircleShape *circle, Body *body, double radius, Vector offset) {
    circle->center = offset;
    circle->radius = radius;

    shapeInit((Shape *) circle, &CircleShapeClass, body, circleShapeMassInfo(0.0f, radius, offset));

    return circle;
}

Shape *circleShapeNew(Body *body, double radius, Vector offset) {
    CircleShape *newCircle = circleShapeAlloc();
    return (Shape *) circleShapeInit(newCircle, body, radius, offset);
}

// Getters:

Vector circleShapeGetOffset(const Shape *shape) {
    assertHard(shape->klass == &CircleShapeClass, VECT_ERR, "Shape is not a circle shape.");

    return ((CircleShape *) shape)->center;
}

double circleShapeGetRadius(const Shape *shape) {
    assertHard(shape->klass == &CircleShapeClass, DBL_ERR, "Shape is not a circle shape.");

    return ((CircleShape *) shape)->radius;
}

// Setters (NOTE: the original program tags these as potentially unsafe):

void circleSetRadius(Shape *shape, const double radius) {
    assertHard(shape->klass == &CircleShapeClass, VOID_ERR, "Shape is not a circle shape.");

    CircleShape *circle = (CircleShape *) shape;
    circle->radius = radius;

    double mass = shape->massInfo.mass;
    shape->massInfo = circleShapeMassInfo(mass, circle->radius, circle->center);
    if (mass > 0.0f) bodyAccumulateMassFromShapes(shape->body);
}

void circleSetOffset(Shape *shape, const Vector offset) {
    assertHard(shape->klass == &CircleShapeClass, VOID_ERR, "Shape is not a circle shape.");

    CircleShape *circle = (CircleShape *) shape;
    circle->center = offset;

    double mass = shape->massInfo.mass;
    shape->massInfo = circleShapeMassInfo(mass, circle->radius, circle->center);
    if (mass > 0.0f) bodyAccumulateMassFromShapes(shape->body);
}
