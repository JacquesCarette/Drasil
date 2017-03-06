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

 // CIRCLE_SHAPE_H - CIRCLE SHAPE MODULE //

#ifndef CIRCLE_SHAPE_H
#define CIRCLE_SHAPE_H

// Inline functions:
// Returns the moment of inertia for a circle shape.
static inline double momentForCircle(const double mass, const double r1, const double r2, const Vector offset) {
    return mass * (0.5f * (r1 * r1 + r2 * r2) + vectLengthSq(offset));
}

// Calculates the area of a circle shape.
static inline double areaForCircle(const double r1, const double r2) {
    return (double) M_PI * fabs(r1 * r1 - r2 * r2);
}

// Constructors for circle shapes.
CircleShape *circleShapeAlloc(void);
CircleShape *circleShapeInit(CircleShape *circle, Body *body, double radius, Vector offset);
Shape *circleShapeNew(Body *body, double radius, Vector offset);

// Getters for circle shapes.
Vector circleShapeGetOffset(const Shape *shape);
double circleShapeGetRadius(const Shape *shape);

// Setters for circle shapes. (NOTE: the original program tags these as potentially unsafe)
void circleSetRadius(Shape *shape, const double radius);
void circleSetOffset(Shape *shape, const Vector offeset);

#endif
