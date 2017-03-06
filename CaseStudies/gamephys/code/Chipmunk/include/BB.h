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

 // BB_H - BOUNDING BOX MODULE //

#ifndef BB_H
#define BB_H

#include <math.h>
#include "../include/Chipmunk.h"
#include "Vector.h"

// Chipmunk's axis-aligned 2D bounding box type.
typedef struct BB {
    double left;
    double bottom;
    double right;
    double top;
} BB;

// Error value for unit testing.
#ifdef UNIT_TEST
    static const BB errorBB = {INT_MAX, INT_MAX, INT_MIN, INT_MIN};
    #define BB_ERR errorBB
#endif

// Constructs a new BB.
static inline BB BBNew(const double left, const double bottom, const double
    right, const double top) {
    BB bb = {left, bottom, right, top};
    return bb;
}

// Constructs a BB around a center point based on the specified half-dimensions.
static inline BB BBNewForExtents(const Vector center, const double hwidth,
    const double hheight) {
    assertWarn(hwidth >= 0 && hheight >= 0, BB_ERR, "Half-dimensions should be nonnegative.");
    return BBNew(center.x - hwidth, center.y - hheight, center.x + hwidth,
        center.y + hheight);
}

// Constructs a new BB for a circle.
static inline BB BBNewForCircle(const Vector pos, const double radius) {
    assertWarn(radius >= 0, BB_ERR, "Radius should be nonnegative.");
    return BBNewForExtents(pos, radius, radius);
}

// Checks if the BBs bb1 and bb2 intersect one another.
static inline bool BBIntersects(const BB bb1, const BB bb2) {
    return (bb1.left <= bb2.right && bb2.left <= bb1.right && bb1.bottom <=
        bb2.top && bb2.bottom <= bb1.top);
}

// Checks if a BB bb contains another BB, other.
static inline bool BBContainsBB(const BB bb, const BB other) {
    return (bb.left <= other.left && other.right <= bb.right && bb.bottom <=
        other.bottom && other.top <= bb.top);
}

// Checks if a BB bb contains a vector v.
static inline bool BBContainsVect(const BB bb, const Vector v) {
    return (bb.left <= v.x && bb.right >= v.x && bb.bottom <= v.y && bb.top >=
        v.y);
}

// Creates a new BB containing both specified BBs.
static inline BB BBMerge(const BB bb1, const BB bb2) {
    return BBNew(
        fmin(bb1.left, bb2.left),
        fmin(bb1.bottom, bb2.bottom),
        fmax(bb1.right, bb2.right),
        fmax(bb1.top, bb2.top)
    );
}

// Returns the centroid of the specified BB.
static inline Vector BBCenter(const BB bb) {
    return vectLerp(vect(bb.left, bb.bottom), vect(bb.right, bb.top), 0.5f);
}

// Returns the area of the specified BB.
static inline double BBArea(const BB bb) {
    return (bb.right - bb.left) * (bb.top - bb.bottom);
}

// Returns the area of the BB containing both given BBs.
static inline double BBMergedArea(const BB bb1, const BB bb2) {
    return (fmax(bb1.right, bb2.right) - fmin(bb1.left, bb2.left)) *
        (fmax(bb1.top, bb2.top) - fmin(bb1.bottom, bb2.bottom));
}

// NOTE: Should segment-related functions be added?
// Won't add for now since I'm avoiding queries altogether for the time being.
// So far it doesn't seem necessary.

// Clamps vector v to the given BB.
static inline Vector BBClampVect(const BB bb, const Vector v) {
    return vect(fclamp(v.x, bb.left, bb.right), fclamp(v.y, bb.bottom, bb.top));
}

// Wraps vector v to the given BB.
static inline Vector BBWrapVect(const BB bb, const Vector v) {
    double dx = fabs(bb.right - bb.left);
    double modx = fmod(v.x - bb.left, dx);
    double x = (modx > 0.0f) ? modx : modx + dx;

    double dy = fabs(bb.top - bb.bottom);
    double mody = fmod(v.y - bb.bottom, dy);
    double y = (mody > 0.0f) ? mody : mody + dy;

    return vect(x + bb.left, y + bb.bottom);
}

// Returns bb offset by the given vector v.
static inline BB BBOffset(const BB bb, const Vector v) {
    return BBNew(
        bb.left + v.x,
        bb.bottom + v.y,
        bb.right + v.y,
        bb.top + v.y
    );
}

#endif
