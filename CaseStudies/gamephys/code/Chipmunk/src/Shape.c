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

#include "../include/Chipmunk.h"

Shape *shapeInit(Shape *shape, const ShapeClass *klass, Body *body, ShapeMassInfo massInfo) {
    shape->klass = klass;

    shape->body = body;
    shape->massInfo = massInfo;

    // shape->sensor = 0;

    shape->elast = 0.0f;
    shape->fric = 0.0f;
    shape->surfaceVel = zeroVect;

    shape->type = 0;

    /* NOTE: Not implemented.
    shape->filter.group = NO_GROUP;
    shape->filter.categories = ALL_CATEGORIES;
    shape->filter.mask = ALL_CATEGORIES;
    */

    shape->space = NULL;

    shape->next = NULL;
    shape->prev = NULL;

    return shape;
}

void shapeDestroy(Shape *shape) {
    if (shape) {
        if (shape->klass && shape->klass->destroy) {
            shape->klass->destroy(shape);
        }
        free(shape);
    }
}

Space *shapeGetSpace(const Shape *shape) {
    return shape->space;
}

Body *shapeGetBody(const Shape *shape) {
    return shape->body;
}

void shapeSetBody(Shape *shape, Body *body) {
    shape->body = body;
}

double shapeGetMass(const Shape *shape) {
    return shape->massInfo.mass;
}

void shapeSetMass(Shape *shape, const double mass) {
    Body *body = shape->body;

    shape->massInfo.mass = mass;
    bodyAccumulateMassFromShapes(body);
}

double shapeGetDensity(const Shape *shape) {
    return shape->massInfo.mass / shape->massInfo.area;
}

void shapeSetDensity(Shape *shape, const double density) {
    shapeSetMass(shape, density * shape->massInfo.area);
}

double shapeGetMoment(const Shape *shape) {
    return shape->massInfo.moment;
}

double shapeGetArea(const Shape *shape) {
    return shape->massInfo.area;
}

Vector shapeGetCenterOfMass(const Shape *shape) {
    return shape->massInfo.com;
}

BB shapeGetBB(const Shape *shape) {
    return shape->bb;
}


double shapeGetElasticity(const Shape *shape) {
    return shape->elast;
}

void shapeSetElasticity(Shape *shape, const double elast) {
    assertHard(elast >= 0.0f, VOID_ERR, "Elasticity must be a positive quantity.");
    shape->elast = elast;
}

double shapeGetFriction(const Shape *shape) {
    return shape->fric;
}

void shapeSetFriction(Shape *shape, const double fric) {
    assertHard(fric >= 0.0f, VOID_ERR, "Friction must be a positive quantity.");
    shape->fric = fric;
}

Vector shapeGetSurfaceVelocity(const Shape *shape) {
    return shape->surfaceVel;
}

void shapeSetSurfaceVelocity(Shape *shape, const Vector surfaceVel) {
    shape->surfaceVel = surfaceVel;
}

CollisionType shapeGetCollisionType(const Shape *shape) {
    return shape->type;
}

void shapeSetCollisionType(Shape *shape, const CollisionType type) {
    shape->type = type;
}

// Set & get ShapeFilter - not implemented.

BB shapeCacheBB(Shape *shape) {
    return shapeUpdate(shape, shape->body->transform);
}

BB shapeUpdate(Shape *shape, Transform transform) {
    return (shape->bb = shape->klass->cacheData(shape, transform));
}
