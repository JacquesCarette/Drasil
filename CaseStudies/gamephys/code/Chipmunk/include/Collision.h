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

// COLLISION_H - COLLISION MODULE //

#ifndef COLLISION_H
#define COLLISION_H

#include "Chipmunk_types.h"

// Collision resolution functions (from chipmunk_private.h):

static inline Vector relative_velocity(Body *a, Body *b, Vector r1, Vector r2) {
    Vector v1sum = vectAdd(a->vel, vectMult(vectPerp(r1), a->avel));
    Vector v2sum = vectAdd(b->vel, vectMult(vectPerp(r2), b->avel));

    return vectSub(v2sum, v1sum);
}

static inline double normal_relative_velocity(Body *a, Body *b, Vector r1,
    Vector r2, Vector n) {
    return vectDot(relative_velocity(a, b, r1, r2), n);
}

static inline void apply_impulse(Body *body, Vector j, Vector r) {
    body->vel = vectAdd(body->vel, vectMult(j, body->massInv));
    body->avel += body->momentInv * vectCross(r, j);
}

static inline void apply_impulses(Body *a, Body *b, Vector r1, Vector r2,
    Vector j) {
    apply_impulse(a, vectNeg(j), r1);
    apply_impulse(b, j, r2);
}

static inline void apply_bias_impulse(Body *body, Vector j, Vector r) {
    body->velBias = vectAdd(body->velBias, vectMult(j, body->massInv));
    body->avelBias += body->momentInv * vectCross(r, j);
}

static inline void apply_bias_impulses(Body *a, Body *b, Vector r1, Vector
    r2, Vector j) {
    apply_bias_impulse(a, vectNeg(j), r1);
    apply_bias_impulse(b, j, r2);
}

static inline double k_scalar_body(Body *body, Vector r, Vector n) {
    double rcn = vectCross(r, n);
    return body->massInv + body->momentInv * rcn * rcn;
}

static inline double k_scalar(Body *a, Body *b, Vector r1, Vector r2, Vector n) {
    double value = k_scalar_body(a, r1, n) + k_scalar_body(b, r2, n);
    assertSoft(value != 0.0, DBL_ERR, "Unsolvable collision or constraint.");

    return value;
}

// Tensors will be added later on as needed.

// Returns collision information about two colliding shapes.
struct CollisionInfo collide(const Shape *a, const Shape *b, CollisionID id,
    struct Contact *contacts);

// Returns contact information about two shapes.
ContactPointSet shapesCollide(const Shape *s1, const Shape *s2);

#endif
