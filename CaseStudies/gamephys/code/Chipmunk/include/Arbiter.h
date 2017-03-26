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

 // ARBITER_H - ARBITER MODULE //

#ifndef ARBITER_H
#define ARBITER_H

// Maximum number of contacts held by an arbiter.
#define MAX_CONTACTS_PER_ARBITER 2

// Getter and setter for the coefficient of restitution of this arbiter.
double arbiterGetRestitution(const Arbiter *arb);
void arbiterSetRestitution(Arbiter *arb, const double restitution);

// Getter and setter for the friction coefficient of this arbiter.
double arbiterGetFriction(const Arbiter *arb);
void arbiterSetFriction(Arbiter *arb, const double friction);

// Getter and setter for the surface velocity of this arbiter.
Vector arbiterGetSurfaceVelocity(const Arbiter *arb);
void arbiterSetSurfaceVelocity(Arbiter *arb, const Vector surfaceVel);

// Calculates the total impulse applied by this arbiter.
Vector arbiterTotalImpulse(const Arbiter *arb);

// Calculates the total kinetic energy applied by this arbiter.
double arbiterTotalKE(const Arbiter *arb);

// mark an arbiter as ignored until the two bodies separate.
bool arbiterIgnore(Arbiter *arb);

// Retrieve the shapes held by this arbiter.
void arbiterGetShapes(const Arbiter *arb, Shape **a, Shape **b);

// Retrieve the bodies held by this arbiter.
void arbiterGetBodies(const Arbiter *arb, Body **a, Body **b);

// The contact point set struct that holds important collision information.
typedef struct ContactPointSet {
    // Number of contact points in the set.
    int count;
    // Collision normal.
    Vector normal;
    // Array of contact points.
    struct {
        // The position of the contact on the surface of each shape.
        Vector pointA;
        Vector pointB;
        // Penetration distance of the two shapes (i.e. if the shapes are
        // overlapping, this quantity is negative). This is calculated as
        // (point1 - point2) dot normal and ignored by
        // arbiterSetContactPointSet()
        double distance;
    } points[MAX_CONTACTS_PER_ARBITER];
} ContactPointSet;

// Getter and setter for the contact point set of an arbiter.
ContactPointSet arbiterGetContactPointSet(const Arbiter *arb);
void arbiterSetContactPointSet(Arbiter *arb, ContactPointSet *set);

// Check if this is the first step a pair of objects started colliding.
bool arbiterIsFirstContact(const Arbiter *arb);

// Check if the separate callback is due to a shape being removed from the space.
bool arbiterIsRemoval(const Arbiter *arb);

// Various selector functions for an arbiter:
// Returns the # of contact points for this arbiter.
int arbiterGetCount(const Arbiter *arb);
// Returns the collision normal.
Vector arbiterGetNormal(const Arbiter *arb);
// Returns the position of the i-th contact point on the first shape.
Vector arbiterGetPointA(const Arbiter *arb, int index);
// Returns the position of the i-th contact point on the second shape.
Vector arbiterGetPointB(const Arbiter *arb, int index);
// Returns the depth of the i-th contact point.
double arbiterGetDepth(const Arbiter *arb, int index);

// Wildcard calls management. Custom callback functions.
// TODO: Remove if not necessary for our scope. Used in Space module, so remove it there too!

bool arbiterCallWildcardBeginA(Arbiter *arb, Space *space);
bool arbiterCallWildcardBeginB(Arbiter *arb, Space *space);

bool arbiterCallWildcardPreSolveA(Arbiter *arb, Space *space);
bool arbiterCallWildcardPreSolveB(Arbiter *arb, Space *space);

void arbiterCallWildcardPostSolveA(Arbiter *arb, Space *space);
void arbiterCallWildcardPostSolveB(Arbiter *arb, Space *space);

void arbiterCallWildcardSeparateA(Arbiter *arb, Space *space);
void arbiterCallWildcardSeparateB(Arbiter *arb, Space *space);

#endif
