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

 // SHAPE_H - SHAPE MODULE //

#ifndef SHAPE_H
#define SHAPE_H

// Constructors and destroyers for a shape.
void shapeDestroy(Shape *shape);

// Update, cache and return the bounding box of the given shape.
BB shapeCacheBB(Shape *shape);
// Update, cache and return the bounding box of a shape with an explicit
// transformation.
BB shapeUpdate(Shape *shape, Transform transform);

// Retrieves the space that the shape is present in.
Space *shapeGetSpace(const Shape *shape);

// Getter and setter for the body the shape is attached to.
Body *shapeGetBody(const Shape *shape);
void shapeSetBody(Shape *shape, Body *body);

// Getter and setter for the mass of the shape.
// NOTE: shapeSetMass resets the mass of the corresponding body accordingly.
double shapeGetMass(const Shape *shape);
void shapeSetMass(Shape *shape, const double mass);

// Getter and setter for the density of the shape.
double shapeGetDensity(const Shape *shape);
void shapeSetDensity(Shape *shape, const double density);

// Returns the moment of inertia of the shape.
// NOTE: shapeSetMass resets the moment of the corresponding body accordingly.
double shapeGetMoment(const Shape *shape);
// Retrieves the area of the shape.
double shapeGetArea(const Shape *shape);
// Retrieves the center of mass of the shape.
Vector shapeGetCenterOfMass(const Shape *shape);

// Retrieves the bounding box of the shape.
BB shapeGetBB(const Shape *shape);

// Getter and setter for the elasticity of the shape.
double shapeGetElasticity(const Shape *shape);
void shapeSetElasticity(Shape *shape, const double elast);

// Getter and setter for the friction of the shape.
double shapeGetFriction(const Shape *shape);
void shapeSetFriction(Shape *shape, const double fric);

// Getter and setter for the surface velocity of the shape.
Vector shapeGetSurfaceVelocity(const Shape *shape);
void shapeSetSurfaceVelocity(Shape *shape, const Vector surfaceVel);

// Getter and setter for the collision type of the shape.
CollisionType shapeGetCollisionType(const Shape *shape);
void shapeSetCollisionType(Shape *shape, const CollisionType type);

#include "CircleShape.h"
#include "SegmentShape.h"
#include "PolyShape.h"

#endif
