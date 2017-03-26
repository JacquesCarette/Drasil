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

// ARBITER_C - ARBITER MODULE //

#include "../include/Chipmunk.h"

static inline void unthreadHelper(Arbiter *arb, Body *body) {
    struct ArbiterThread *thread = arbiterThreadForBody(arb, body);
    Arbiter *prev = thread->prev;
    Arbiter *next = thread->next;

    if (prev) {
        arbiterThreadForBody(prev, body)->next = next;
    } else if (body->arbiterList == arb) {
        // This means arb is at the front (head) of the arbiterList
        // This function may be called for an arbiter that was never in a list
        // In that case, we need to protect it from wiping out the
        // body->arbiterList pointer.
        body->arbiterList = next;
    }

    if (next) arbiterThreadForBody(next, body)->prev = prev;

    thread->prev = NULL;
    thread->next = NULL;
}

void arbiterUnthread(Arbiter *arb) {
    unthreadHelper(arb, arb->bodyA);
    unthreadHelper(arb, arb->bodyB);
}

bool arbiterIsFirstContact(const Arbiter *arb) {
    return arb->state == ARBITER_STATE_FIRST_COLLISION;
}

bool arbiterIsRemoval(const Arbiter *arb) {
    return arb->state == ARBITER_STATE_INVALIDATED;
}

int arbiterGetCount(const Arbiter *arb) {
    // Return 0 contacts if we are in a separate callback.
    return (arb->state < ARBITER_STATE_CACHED ? arb->count : 0);
}

Vector arbiterGetNormal(const Arbiter *arb) {
    return vectMult(arb->normal, arb->swapped ? -1.0f : 1.0);
}

Vector arbiterGetPointA(const Arbiter *arb, int i) {
    assertHard(0 <= i && i < arbiterGetCount(arb), VECT_ERR, "Index error: The specified contact index is invalid for this arbiter/");
    return vectAdd(arb->bodyA->pos, arb->contacts[i].r1);
}

Vector arbiterGetPointB(const Arbiter *arb, int i) {
    assertHard(0 <= i && i < arbiterGetCount(arb), VECT_ERR, "Index error: The specified contact index is invalid for this arbiter.");
    return vectAdd(arb->bodyB->pos, arb->contacts[i].r2);
}

double arbiterGetDepth(const Arbiter *arb, int i) {
    assertHard(0 <= i && i < arbiterGetCount(arb), DBL_ERR, "Index error: The specified contact index is invalid for this arbiter.");
    struct Contact *con = &arb->contacts[i];
    return vectDot(vectAdd(vectSub(con->r2, con->r1), vectSub(arb->bodyB->pos, arb->bodyA->pos)), arb->normal);
}

ContactPointSet arbiterGetContactPointSet(const Arbiter *arb) {
    ContactPointSet set;
    set.count = arbiterGetCount(arb);

    bool swapped = arb->swapped;
    Vector normal = arb->normal;
    set.normal = (swapped ? vectNeg(normal) : normal);

    for (int i = 0; i < set.count; ++i) {
        // Contact points are relative to body CoMs.
        Vector p1 = vectAdd(arb->bodyA->pos, arb->contacts[i].r1);
        Vector p2 = vectAdd(arb->bodyB->pos, arb->contacts[i].r2);

        set.points[i].pointA = (swapped ? p2 : p1);
        set.points[i].pointB = (swapped ? p1 : p2);
        set.points[i].distance = vectDot(vectSub(p2, p1), normal);
    }

    return set;
}

void arbiterSetContactPointSet(Arbiter *arb, ContactPointSet *set) {
    int count = set->count;
    assertHard(count == arb->count, VOID_ERR, "The number of contact points cannot be changed.");

    bool swapped = arb->swapped;
    arb->normal = (swapped ? vectNeg(set->normal) : set->normal);

    for (int i = 0; i < count; ++i) {
        // Convert back to CoM relative offsets.
        Vector p1 = set->points[i].pointA;
        Vector p2 = set->points[i].pointB;

        arb->contacts[i].r1 = vectSub(swapped ? p2 : p1, arb->bodyA->pos);
        arb->contacts[i].r2 = vectSub(swapped ? p1 : p2, arb->bodyB->pos);
    }
}

Vector arbiterTotalImpulse(const Arbiter *arb) {
    struct Contact *contacts = arb->contacts;
    Vector normal = arb->normal;
    Vector sum = zeroVect;

    for (int i = 0, count = arbiterGetCount(arb); i < count; ++i) {
        struct Contact *con = &contacts[i];
        sum = vectAdd(sum, vectRotate(normal, vect(con->jnAcc, con->jtAcc)));
    }

    return (arb->swapped ? sum : vectNeg(sum));
    // return zeroVect; Not sure why this 2nd return statement was here.
}

double arbiterTotalKE(const Arbiter *arb) {
    double restCoef = (1 - arb->elast) / (1 + arb->elast);
    double sum = 0.0;

    struct Contact *contacts = arb->contacts;
    for (int i = 0, count = arbiterGetCount(arb); i < count; ++i) {
        struct Contact *con = &contacts[i];
        double jnAcc = con->jnAcc;
        double jtAcc = con->jtAcc;

        sum += restCoef * jnAcc * jnAcc / con->nMass + jtAcc * jtAcc / con->tMass;
    }

    return sum;
}

bool arbiterIgnore(Arbiter *arb) {
    arb->state = ARBITER_STATE_IGNORE;
    return false;
}

double arbiterGetRestitution(const Arbiter *arb) {
    return arb->elast;
}

void arbiterSetRestitution(Arbiter *arb, const double restitution) {
    arb->elast = restitution;
}

double arbiterGetFriction(const Arbiter *arb) {
    return arb->fric;
}

void arbiterSetFriction(Arbiter *arb, const double friction) {
    arb->fric = friction;
}

Vector arbiterGetSurfaceVelocity(const Arbiter *arb) {
    return vectMult(arb->surfaceVel, arb->swapped ? -1.0f : 1.0);
}

void arbiterSetSurfaceVelocity(Arbiter *arb, const Vector surfaceVel) {
    arb->surfaceVel = (arb->swapped ? vectNeg(surfaceVel) : surfaceVel);
}

void arbiterGetShapes(const Arbiter *arb, Shape **a, Shape **b) {
    if (arb->swapped) {
        (*a) = (Shape *) arb->b, (*b) = (Shape *) arb->a;
    } else {
        (*a) = (Shape *) arb->a, (*b) = (Shape *) arb->b;
    }
}

void arbiterGetBodies(const Arbiter *arb, Body **a, Body **b) {
    Shape *shapeA;
    Shape *shapeB;

    arbiterGetShapes(arb, &shapeA, &shapeB);
    (*a) = shapeA->body;
    (*b) = shapeB->body;
}

// NOTE: Wildcard calls management. Not sure if necessary.

bool arbiterCallWildcardBeginA(Arbiter *arb, Space *space) {
    CollisionHandler *handler = arb->handlerA;
    return handler->beginFunc(arb, space, handler->userData);
}

bool arbiterCallWildcardBeginB(Arbiter *arb, Space *space) {
    CollisionHandler *handler = arb->handlerB;
    arb->swapped = !arb->swapped;
    bool retval = handler->beginFunc(arb, space, handler->userData);
    arb->swapped = !arb->swapped;
    return retval;
}

bool arbiterCallWildcardPreSolveA(Arbiter *arb, Space *space) {
    CollisionHandler *handler = arb->handlerA;
    return handler->preSolveFunc(arb, space, handler->userData);
}

bool arbiterCallWildcardPreSolveB(Arbiter *arb, Space *space) {
    CollisionHandler *handler = arb->handlerB;
    arb->swapped = !arb->swapped;
    bool retval = handler->preSolveFunc(arb, space, handler->userData);
    arb->swapped = !arb->swapped;
    return retval;
}

void arbiterCallWildcardPostSolveA(Arbiter *arb, Space *space) {
    CollisionHandler *handler = arb->handlerA;
    handler->postSolveFunc(arb, space, handler->userData);
}

void arbiterCallWildcardPostSolveB(Arbiter *arb, Space *space) {
    CollisionHandler *handler = arb->handlerB;
    arb->swapped = !arb->swapped;
    handler->postSolveFunc(arb, space, handler->userData);
    arb->swapped = !arb->swapped;
}

void arbiterCallWildcardSeparateA(Arbiter *arb, Space *space) {
    CollisionHandler *handler = arb->handlerA;
    handler->separateFunc(arb, space, handler->userData);
}

void arbiterCallWildcardSeparateB(Arbiter *arb, Space *space) {
    CollisionHandler *handler = arb->handlerB;
    arb->swapped = !arb->swapped;
    handler->separateFunc(arb, space, handler->userData);
    arb->swapped = !arb->swapped;
}

Arbiter *arbiterInit(Arbiter *arb, Shape *a, Shape *b) {
    arb->handler = NULL;
    arb->swapped = false;

    arb->handlerA = NULL;
    arb->handlerB = NULL;

    arb->elast = 0.0f;
    arb->fric = 0.0f;
    arb->surfaceVel = zeroVect;

    arb->count = 0;
    arb->contacts = NULL;

    arb->a = a; arb->bodyA = a->body;
    arb->b = b; arb->bodyB = b->body;

    arb->threadA.next = NULL;
    arb->threadB.next = NULL;
    arb->threadA.prev = NULL;
    arb->threadB.prev = NULL;

    arb->stamp = 0;
    arb->state = ARBITER_STATE_FIRST_COLLISION;

    return arb;
}

static inline CollisionHandler *spaceLookupHandler(Space *space, CollisionType a, CollisionType b, CollisionHandler *defaultValue) {
    CollisionType types[] = {a, b};
    CollisionHandler *handler = (CollisionHandler *) hashSetFind(space->collisionHandlers, HASH_PAIR(a, b), types);
    return (handler ? handler : defaultValue);
}

void arbiterUpdate(Arbiter *arb, struct CollisionInfo *info, Space *space) {
    const Shape *a = info->a;
    const Shape *b = info->b;

    // For collisions between two similar primitive types, the order could have
    // been swapped since the last frame

    arb->a = a;
    arb->bodyA = a->body;
    arb->b = b;
    arb->bodyB = b->body;

    // Iterate over possible pairs to look for hash value matches
    for (int i = 0; i < info->count; ++i) {
        struct Contact *con = &info->arr[i];

        // r1 and r2 store absolute offsets at init time - need to convert
        // them to relative offsets
        con->r1 = vectSub(con->r1, a->body->pos);
        con->r2 = vectSub(con->r2, b->body->pos);

        // Cached impulses are not zeroed at init time
        con->jnAcc = 0.0f;
        con->jtAcc = 0.0f;

        for (int j = 0; j < arb->count; ++j) {
            struct Contact *old = &arb->contacts[j];

            // This could trigger false positives, but is fairly unlikely to
            // be serious if it does
            if (con->hash == old->hash) {
                // Copy the persistent contact information.
                con->jnAcc = old->jnAcc;
                con->jtAcc = old->jtAcc;
            }
        }
    }

    arb->contacts = info->arr;
    arb->count = info->count;
    arb->normal = info->normal;

    arb->elast = a->elast * b->elast;
    arb->fric = a->fric * b->fric;

    Vector surfaceVel = vectSub(b->surfaceVel, a->surfaceVel);
    arb->surfaceVel = vectSub(surfaceVel, vectMult(info->normal, vectDot(surfaceVel, info->normal)));

    CollisionType typeA = info->a->type;
    CollisionType typeB = info->b->type;
    CollisionHandler *defaultHandler = &space->defaultHandler;
    CollisionHandler *handler = spaceLookupHandler(space, typeA, typeB, defaultHandler);
    arb->handler = handler;

    // Check if the types match, but don't swap for a default handler which use the wildcard for type A.
    bool swapped = (typeA != handler->typeA && handler->typeA != WILDCARD_COLLISION_TYPE);
    arb->swapped = swapped;

    if (handler != defaultHandler || space->usesWildcards) {
        // The order of the main handler swaps the wildcard handlers too.
        arb->handlerA = spaceLookupHandler(space, (swapped ? typeB : typeA), WILDCARD_COLLISION_TYPE, &collisionHandlerDoNothing);
        arb->handlerB = spaceLookupHandler(space, (swapped ? typeA : typeB), WILDCARD_COLLISION_TYPE, &collisionHandlerDoNothing);
    }

    // Finally, mark as new if it has been cached
    if (arb->state == ARBITER_STATE_CACHED) {
        arb->state = ARBITER_STATE_FIRST_COLLISION;
    }
}

void arbiterPreStep(Arbiter *arb, double dt, double slop, double bias) {
    Body *a = arb->bodyA;
    Body *b = arb->bodyB;
    Vector normal = arb->normal;
    Vector bodyDelta = vectSub(b->pos, a->pos);

    for (int i = 0; i < arb->count; ++i) {
        struct Contact *con = &arb->contacts[i];

        // Calculate the mass normal and mass tangent.
        con->nMass = 1.0f / k_scalar(a, b, con->r1, con->r2, normal);
        con->tMass = 1.0f / k_scalar(a, b, con->r1, con->r2, vectPerp(normal));

        // Calculate the target bias velocity.
        double dist = vectDot(vectAdd(vectSub(con->r2, con->r1), bodyDelta), normal);
        con->bias = -bias * fmin(0.0f, dist + slop) / dt;
        con->jBias = 0.0f;

        // Calculate the target bounce velocity.
        con->bounce = normal_relative_velocity(a, b, con->r1, con->r2, normal) * arb->elast;
    }
}

void arbiterApplyCachedImpulse(Arbiter *arb, double dtCoef) {
    if (arbiterIsFirstContact(arb)) return;

    Body *a = arb->bodyA;
    Body *b = arb->bodyB;
    Vector normal = arb->normal;

    for (int i = 0; i < arb->count; i++) {
        struct Contact *con = &arb->contacts[i];
        Vector j = vectRotate(normal, vect(con->jnAcc, con->jtAcc));
        apply_impulses(a, b, con->r1, con->r2, vectMult(j, dtCoef));
    }
}

void arbiterApplyImpulse(Arbiter *arb) {
    Body *a = arb->bodyA;
    Body *b = arb->bodyB;
    Vector normal = arb->normal;
    Vector surfaceVel = arb->surfaceVel;
    double friction = arb->fric;

    for (int i = 0; i < arb->count; ++i) {
        struct Contact *con = &arb->contacts[i];
        double nMass = con->nMass;
        Vector r1 = con->r1;
        Vector r2 = con->r2;

        Vector vb1 = vectAdd(a->velBias, vectMult(vectPerp(r1), a->avelBias));
        Vector vb2 = vectAdd(b->velBias, vectMult(vectPerp(r2), b->avelBias));
        Vector vr = vectAdd(relative_velocity(a, b, r1, r2), surfaceVel);

        double vbn = vectDot(vectSub(vb2, vb1), normal);
        double vrn = vectDot(vr, normal);
        double vrt = vectDot(vr, vectPerp(normal));

        double jbn = (con->bias - vbn) * nMass;
        double jbnOld = con->jBias;
        con->jBias = fmax(jbnOld + jbn, 0.0f);

        double jn = -(con->bounce + vrn) * nMass;
        double jnOld = con->jnAcc;
        con->jnAcc = fmax(jnOld + jn, 0.0f);

        double jtMax = friction * con->jnAcc;
        double jt = -vrt * con->tMass;
        double jtOld = con->jtAcc;
        con->jtAcc = fclamp(jtOld + jt, -jtMax, jtMax);

        apply_bias_impulses(a, b, r1, r2, vectMult(normal, con->jBias - jbnOld));
        apply_impulses(a, b, r1, r2, vectRotate(normal, vect(con->jnAcc - jnOld, con->jtAcc - jtOld)));
    }
}
