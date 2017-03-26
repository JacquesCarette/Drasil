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

 // SEGMENT_SHAPE_H - SEGMENT SHAPE MODULE //

#ifndef SEGMENT_SHAPE_H
#define SEGMENT_SHAPE_H

// Inline functions:
// Returns the moment of inertia for a segment shape.
static inline double momentForSegment(const double mass, const Vector a, const Vector b, const double radius) {
    Vector offset = vectLerp(a, b, 0.5f);
    double length = vectDist(b, a) + 2.0f * radius;
    return mass * ((length * length + 4.0f * radius * radius) / 12.0f + vectLengthSq(offset));
}

// Calculates the area of a segment shape.
static inline double areaForSegment(const Vector a, const Vector b, const double radius) {
    return radius * ((double) M_PI * radius + 2.0f * vectDist(a, b));
}

// Constructors for segment shapes.
SegmentShape *segmentShapeAlloc(void);
SegmentShape *segmentShapeInit(SegmentShape *seg, Body *body, Vector a, Vector b, double radius);
Shape *segmentShapeNew(Body *body, Vector a, Vector b, double radius);

// Getters for segment shapes.
Vector segmentShapeGetA(const Shape *shape);
Vector segmentShapeGetB(const Shape *shape);
Vector segmentShapeGetNormal(const Shape *shape);
double segmentShapeGetRadius(const Shape *shape);

// Setters for segment shapes. (NOTE: the last two are tagged unsafe, as with the circle ones)
void segmentShapeSetNeighbors(Shape *shape, Vector prev, Vector next);

void segmentShapeSetEndpoints(Shape *shape, const Vector a, const Vector b);
void segmentShapeSetRadius(Shape *shape, const double radius);

#endif
