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

 // SPACE_C - SPACE MODULE //

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../include/Chipmunk.h"

// Contact Set Helpers:

// Checks equality for arbiterSets.
static bool arbiterSetEql(Shape **shapes, Arbiter *arb) {
    Shape *a = shapes[0];
    Shape *b = shapes[1];

    return ((a == arb->a && b == arb->b) || (b == arb->a && a == arb->b));
}

// Collision Handler Set Helpers:

// Checks equality for collision handlers.
static bool handlerSetEql(CollisionHandler *check, CollisionHandler *pair) {
    return (
        (check->typeA == pair->typeA && check->typeB == pair->typeB) ||
        (check->typeB == pair->typeA && check->typeA == pair->typeB)
    );
}

// Transformation function for collision handlers.
static void *handlerSetTrans(CollisionHandler *handler, void *unused) {
    CollisionHandler *copy = (CollisionHandler *) calloc(1, sizeof(CollisionHandler));
    memcpy(copy, handler, sizeof(CollisionHandler));

    return copy;
}

// Misc Helpers:

// NOTE: Default collision handler - added for completeness. Code in this portion will be added/removed as necessary.

static bool defaultBegin(Arbiter *arb, Space *space, void *data) {
    bool retA = arbiterCallWildcardBeginA(arb, space);
    bool retB = arbiterCallWildcardBeginB(arb, space);
    return retA && retB;
}

static bool defaultPreSolve(Arbiter *arb, Space *space, void *data) {
    bool retA = arbiterCallWildcardPreSolveA(arb, space);
    bool retB = arbiterCallWildcardPreSolveB(arb, space);
    return retA && retB;
}

static void defaultPostSolve(Arbiter *arb, Space *space, void *data) {
    arbiterCallWildcardPostSolveA(arb, space);
    arbiterCallWildcardPostSolveB(arb, space);
}

static void defaultSeparate(Arbiter *arb, Space *space, void *data) {
    arbiterCallWildcardSeparateA(arb, space);
    arbiterCallWildcardSeparateB(arb, space);
}

static CollisionHandler collisionHandlerDefault = {
    WILDCARD_COLLISION_TYPE,
    WILDCARD_COLLISION_TYPE,
    defaultBegin,
    defaultPreSolve,
    defaultPostSolve,
    defaultSeparate,
    NULL
};

static bool alwaysCollide(Arbiter *arb, Space *space, void *data) {
    return true;
}
static void doNothing(Arbiter *arb, Space *space, void *data){}

CollisionHandler collisionHandlerDoNothing = {
    WILDCARD_COLLISION_TYPE,
    WILDCARD_COLLISION_TYPE,
    alwaysCollide,
    alwaysCollide,
    doNothing,
    doNothing,
    NULL
};

static Vector shapeVelocityFunc(Shape *shape) {return shape->body->vel;}

static void freeWrap(void *ptr, void *unused) {free(ptr);}

// Main functions:

Space *spaceAlloc(void) {
    return (Space *) calloc(1, sizeof(Space));
}

Space *spaceInit(Space *space) {
    space->iterations = 10;

    space->gravity = zeroVect;

    space->collisionSlop = 0.1f;
    space->collisionBias = pow(1.0f - 0.1f, 60.0f);
    space->collisionPersistence = 3;

    space->locked = 0;
    space->stamp = 0;

    space->shapeIDCounter = 0;
    space->staticShapes = BBTreeNew((SpatialIndexBBFunc) shapeGetBB, NULL);
    space->dynamicShapes = BBTreeNew((SpatialIndexBBFunc) shapeGetBB, space->staticShapes);
    BBTreeSetVelocityFunc(space->dynamicShapes, (BBTreeVelocityFunc) shapeVelocityFunc);


    space->allocatedBuffers = arrayNew(0);

    space->dynamicBodies = arrayNew(0);
    space->staticBodies = arrayNew(0);

    space->arbiters = arrayNew(0);
    space->pooledArbiters = arrayNew(0);

    space->contactBuffersHead = NULL;
    space->cachedArbiters = hashSetNew(0, (HashSetEqlFunc) arbiterSetEql);

    space->usesWildcards = false;
    memcpy(&space->defaultHandler, &collisionHandlerDoNothing, sizeof(CollisionHandler));
    space->collisionHandlers = hashSetNew(0, (HashSetEqlFunc) handlerSetEql);

    space->postStepCallbacks = arrayNew(0);
    space->skipPostStep = false;

    Body *staticBody = bodyInit(&space->_staticBody, 0.0f, 0.0f);
    bodySetType(staticBody, STATIC_BODY);
    spaceSetStaticBody(space, staticBody);

    return space;
}

Space *spaceNew(void) {
    return spaceInit(spaceAlloc());
}

void spaceDestroy(Space *space) {
    spatialIndexFree(space->staticShapes);
    spatialIndexFree(space->dynamicShapes);

    arrayFree(space->dynamicBodies);
    arrayFree(space->staticBodies);

    hashSetFree(space->cachedArbiters);

    arrayFree(space->arbiters);
    arrayFree(space->pooledArbiters);

    if (space->allocatedBuffers) {
        arrayFreeEach(space->allocatedBuffers, free);
        arrayFree(space->allocatedBuffers);
    }

    if (space->postStepCallbacks) {
        arrayFreeEach(space->postStepCallbacks, free);
        arrayFree(space->postStepCallbacks);
    }

    if (space->collisionHandlers) {
        hashSetEach(space->collisionHandlers, freeWrap, NULL);
        hashSetFree(space->collisionHandlers);
    }
}

void spaceFree(Space *space) {
    if (space) {
        spaceDestroy(space);
        free(space);
    }
}

// Basic getters and setters:

int spaceGetIterations(const Space *space) {
    return space->iterations;
}

void spaceSetIterations(Space *space, int iterations) {
    assertHard(iterations > 0, VOID_ERR, "Iterations must be positive and non-zero.");
    space->iterations = iterations;
}

Vector spaceGetGravity(const Space *space) {
    return space->gravity;
}

void spaceSetGravity(Space *space, Vector gravity) {
    space->gravity = gravity;
}

double spaceGetCollisionSlop(const Space *space) {
    return space->collisionSlop;
}

void spaceSetCollisionSlop(Space *space, double collisionSlop) {
    space->collisionSlop = collisionSlop;
}

double spaceGetCollisionBias(const Space *space) {
    return space->collisionBias;
}

void spaceSetCollisionBias(Space *space, double collisionBias) {
    space->collisionBias = collisionBias;
}

Timestamp spaceGetCollisionPersistence(const Space *space) {
    return space->collisionPersistence;
}

void spaceSetCollisionPersistence(Space *space, Timestamp collisionPersistence) {
    space->collisionPersistence = collisionPersistence;
}

double spaceGetCurrentTimeStep(const Space *space) {
    return space->curr_dt;
}


Body *spaceGetStaticBody(const Space *space) {
    return space->staticBody;
}

void spaceSetStaticBody(Space *space, Body *body) {
    if(space->staticBody){
		assertHard(space->staticBody->shapeList == NULL, VOID_ERR, "Internal Error: Changing the designated static body while the old one still had shapes attached.");
		space->staticBody->space = NULL;
	}

    space->staticBody = body;
    body->space = space;
}

bool spaceIsLocked(const Space *space) {
    return (space->locked > 0);
}

// Collision Handler management functions:
// NOTE: Will be removed if not needed.

static void spaceUseWildcardDefaultHandler(Space *space) {
    if (!space->usesWildcards) {
        space->usesWildcards = true;
        memcpy(&space->defaultHandler, &collisionHandlerDefault, sizeof(CollisionHandler));
    }
}

CollisionHandler *spaceAddDefaultCollisionHandler(Space *space) {
    spaceUseWildcardDefaultHandler(space);
    return &space->defaultHandler;
}

CollisionHandler *spaceAddCollisionHandler(Space *space, CollisionType a, CollisionType b) {
    HashValue hash = HASH_PAIR(a, b);
    CollisionHandler handler = {a, b, defaultBegin, defaultPreSolve, defaultPostSolve, defaultSeparate, NULL};
    return (CollisionHandler *) hashSetInsert(space->collisionHandlers, hash, &handler, (HashSetTransFunc) handlerSetTrans, NULL);
}

CollisionHandler *spaceAddWildcardHandler(Space *space, CollisionType type) {
    spaceUseWildcardDefaultHandler(space);

    HashValue hash = HASH_PAIR(type, WILDCARD_COLLISION_TYPE);
    CollisionHandler handler = {type, WILDCARD_COLLISION_TYPE,
    alwaysCollide, alwaysCollide, doNothing, doNothing, NULL};
    return (CollisionHandler *) hashSetInsert(space->collisionHandlers, hash, &handler, (HashSetTransFunc) handlerSetTrans, NULL);
}

// Body and Shape management functions:

Shape *spaceAddShape(Space *space, Shape *shape) {
    Body *body = shape->body;

    assertHard(shape->space != space, PTR_ERR, "You have already added this shape to this space. You must not add it a second time.");
	assertHard(!shape->space, PTR_ERR, "You have already added this shape to another space. You cannot add it to a second.");
	assertSpaceUnlocked(space);

    bool isStatic = (bodyGetType(body) == STATIC_BODY);
    bodyAddShape(body, shape);

    shape->hashId = space->shapeIDCounter++;
    shapeUpdate(shape, body->transform);
    spatialIndexInsert(isStatic ? space->staticShapes : space->dynamicShapes,
    shape, shape->hashId);
    shape->space = space;

    return shape;
}

Body *spaceAddBody(Space *space, Body *body) {
    assertHard(body->space != space, PTR_ERR, "You have already added this body to this space. You must not add it a second time.");
	assertHard(!body->space, PTR_ERR, "You have already added this body to another space. You cannot add it to a second.");
	assertSpaceUnlocked(space);

    arrayPush(spaceArrayForBodyType(space, bodyGetType(body)), body);

    return body;
}

// Context structure for arbiter filtering.
struct ArbiterFilterContext {
    Space *space;
    Body *body;
    Shape *shape;
};

static bool cachedArbitersFilter(Arbiter *arb, struct ArbiterFilterContext *context) {
    Shape *shape = context->shape;
    Body *body = context->body;

    // Match on the filter shape or if it's NULL the filter body.
    if (
        (body == arb->bodyA && (shape == arb->a || shape == NULL)) ||
        (body == arb->bodyB && (shape == arb->b || shape == NULL))
    ) {
        // Call separate when removing shapes.
        if (shape && arb->state != ARBITER_STATE_CACHED) {
            // Invalidate arbiter since one of the shapes was removed.
            arb->state = ARBITER_STATE_INVALIDATED;

            CollisionHandler *handler = arb->handler;
            handler->separateFunc(arb, context->space, handler->userData);
        }

        arbiterUnthread(arb);
        arrayDeleteObj(context->space->arbiters, arb);
        arrayPush(context->space->pooledArbiters, arb);

        return false;
    }

    return true;
}

void spaceFilterArbiters(Space *space, Body *body, Shape *filter) {
    spaceLock(space); {
        struct ArbiterFilterContext context = {space, body, filter};
        hashSetFilter(space->cachedArbiters, (HashSetFilterFunc) cachedArbitersFilter, &context);
    } spaceUnlock(space, true);
}

void spaceRemoveShape(Space *space, Shape *shape) {
    Body *body = shape->body;
	assertHard(spaceContainsShape(space, shape), VOID_ERR, "Cannot remove a shape that was not added to the space. (Removed twice maybe?)");
	assertSpaceUnlocked(space);

    bodyRemoveShape(body, shape);
    spaceFilterArbiters(space, body, shape);
    spatialIndexRemove((bodyGetType(body)) == STATIC_BODY ? space->staticShapes : space->dynamicShapes, shape, shape->hashId);
    shape->space = NULL;
    shape->hashId = 0;
}

void spaceRemoveBody(Space *space, Body *body) {
    assertHard(body != spaceGetStaticBody(space), VOID_ERR, "Cannot remove the designated static body for the space.");
	assertHard(spaceContainsBody(space, body), VOID_ERR, "Cannot remove a body that was not added to the space. (Removed twice maybe?)");
	assertSpaceUnlocked(space);

    arrayDeleteObj(spaceArrayForBodyType(space, bodyGetType(body)), body);
    body->space = NULL;
}

bool spaceContainsShape(Space *space, Shape *shape) {
    return (shape->space == space);
}

bool spaceContainsBody(Space *space, Body *body) {
    return (body->space == space);
}

// Iterators:

void spaceEachBody(Space *space, SpaceBodyIteratorFunc func, void *data) {
    spaceLock(space); {
        Array *bodies = space->dynamicBodies;
        for (int i = 0; i < bodies->num; ++i) {
            func((Body *) bodies->arr[i], data);
        }

        Array *otherBodies = space->staticBodies;
        for (int i = 0; i < otherBodies->num; ++i) {
            func((Body *) otherBodies->arr[i], data);
        }
    } spaceUnlock(space, true);
}

// Context structure for shape iterators.
typedef struct SpaceShapeContext {
    SpaceShapeIteratorFunc func;
    void *data;
} SpaceShapeContext;

static void spaceEachShapeIterator(Shape *shape, SpaceShapeContext *context) {
    context->func(shape, context->data);
}

void spaceEachShape(Space *space, SpaceShapeIteratorFunc func, void *data) {
    spaceLock(space); {
        SpaceShapeContext context = {func, data};
        spatialIndexEach(space->dynamicShapes,
        (SpatialIndexIteratorFunc) spaceEachShapeIterator, &context);
        spatialIndexEach(space->staticShapes,
        (SpatialIndexIteratorFunc) spaceEachShapeIterator, &context);
    } spaceUnlock(space, true);
}

// Spatial index management functions:

void spaceReindexStatic(Space *space) {
    assertHard(!space->locked, VOID_ERR, "You cannot manually reindex objects while the space is locked. Wait until the current query or step is complete.");

    spatialIndexEach(space->staticShapes,
    (SpatialIndexIteratorFunc) &shapeUpdateFunc, NULL);
    spatialIndexReindex(space->staticShapes);
}

void spaceReindexShape(Space *space, Shape *shape) {
    assertHard(!space->locked, VOID_ERR, "You cannot manually reindex objects while the space is locked. Wait until the current query or step is complete.");

    shapeCacheBB(shape);

    // Attempt to rehash the shape in both hashes.
    spatialIndexReindexObject(space->dynamicShapes, shape, shape->hashId);
    spatialIndexReindexObject(space->staticShapes, shape, shape->hashId);
}

void spaceReindexShapesForBody(Space *space, Body *body) {
    BODY_FOREACH_SHAPE(body, shape) spaceReindexShape(space, shape);
}

// NOTE: Spatial hash functions are not implemented. The current implementation
// only uses AABB trees for collision detection.
