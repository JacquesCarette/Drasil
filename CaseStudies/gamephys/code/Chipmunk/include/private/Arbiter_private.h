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

// ARBITER PRIVATE DATA //

#ifndef ARBITER_PRIVATE_H
#define ARBITER_PRIVATE_H

#include "../Chipmunk_types.h"

// States of an arbiter.
typedef enum ArbiterState {
    // Arbiter is active and it's the first collision.
    ARBITER_STATE_FIRST_COLLISION,
    // Arbiter is active, but it's not the first collision.
    ARBITER_STATE_NORMAL,
    // Collision has been explicitly ignored by:
    // - Returning false from a begin collision handler, or
    // - Calling arbiterIgnore().
    ARBITER_STATE_IGNORE,
    // Collision is no longer active. A space will cache an
    // arbiter for up to space.collisionPersistence more steps.
    ARBITER_STATE_CACHED,
    // Collision arbiter is invalid because one of the shapes was
    // removed.
    ARBITER_STATE_INVALIDATED,
} ArbiterState;

// Structure definition for an arbiter thread.
struct ArbiterThread {
    struct Arbiter *next;
    struct Arbiter *prev;
};

// Structure for a contact point.
struct Contact {
    Vector r1;
    Vector r2;

    double nMass;
    double tMass;
    double bounce; // Creator's TODO: look for an alternate bounce solution.

    double jnAcc;
    double jtAcc;
    double jBias;
    double bias;

    HashValue hash;
};

// Collision information structure definition.
struct CollisionInfo {
    const Shape *a;
    const Shape *b;
    CollisionID id;

    Vector normal;

    int count;
    struct Contact *arr;
};

// Arbiter structure definition.
struct Arbiter {
    double elast;
    double fric;
    Vector surfaceVel;

    const Shape *a;
    const Shape *b;
    Body *bodyA;
    Body *bodyB;
    struct ArbiterThread threadA;
    struct ArbiterThread threadB;

    int count;
    struct Contact *contacts;
    Vector normal;

    // Regular, wildcard A and wildcard B collision handlers
    CollisionHandler *handler;
    CollisionHandler *handlerA;
    CollisionHandler *handlerB;
    bool swapped;

    Timestamp stamp;
    ArbiterState state;
};

// Initializes an arbiter.
Arbiter *arbiterInit(Arbiter *arb, Shape *a, Shape *b);

static inline struct ArbiterThread *arbiterThreadForBody(Arbiter *arb, Body *body) {
    return (arb->bodyA == body ? &arb->threadA : &arb->threadB);
}

// Unthread an arbiter.
void arbiterUnthread(Arbiter *arb);

// Updates the information contained in an arbiter based on the given collision
// information.
void arbiterUpdate(Arbiter *arb, struct CollisionInfo *info, Space *space);

// Calculates values used by the collision solver.
void arbiterPreStep(Arbiter *arb, double dt, double slop, double bias);
void arbiterApplyCachedImpulse(Arbiter *arb, double dtCoef);

// Applies an impulse to the bodies in this arbiter.
void arbiterApplyImpulse(Arbiter *arb);

static inline Arbiter* arbiterNext(Arbiter *node, Body *body) {
    return (node->bodyA == body ? node->threadA.next : node->threadB.next);
}

#define BODY_FOREACH_ARBITER(body, var)\
for (Arbiter *var = body->arbiterList; var; var = arbiterNext(var, body))

#endif
