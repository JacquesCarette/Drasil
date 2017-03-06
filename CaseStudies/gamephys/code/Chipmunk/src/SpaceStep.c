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

 // SPACE_STEP_C - SPACE STEP MODULE //

#include "../include/Chipmunk.h"

// NOTE: Post-step callback functions currently not implemented.

// Contact buffer header.
struct ContactBufferHeader {
    Timestamp stamp;
    ContactBufferHeader *next;
    unsigned int numContacts;
};

#define CONTACTS_BUFFER_SIZE ((BUFFER_BYTES - sizeof(ContactBufferHeader)) / sizeof(struct Contact))

typedef struct ContactBuffer {
    ContactBufferHeader header;
    struct Contact contacts[CONTACTS_BUFFER_SIZE];
} ContactBuffer;

static ContactBufferHeader *spaceAllocContactBuffer(Space *space) {
    ContactBuffer *buffer = (ContactBuffer *) calloc(1, sizeof(ContactBuffer));
    arrayPush(space->allocatedBuffers, buffer);
    return (ContactBufferHeader *) buffer;
}

static ContactBufferHeader *contactBufferHeaderInit(ContactBufferHeader *header, Timestamp stamp, ContactBufferHeader *splice) {
    header->stamp = stamp;
    header->next = (splice ? splice->next : header);
    header->numContacts = 0;

    return header;
}

void spacePushFreshContactBuffer(Space *space) {
    Timestamp stamp = space->stamp;

    ContactBufferHeader *head = space->contactBuffersHead;

    if (!head) {
        // No buffers have been allocated, make one.
        space->contactBuffersHead = contactBufferHeaderInit(spaceAllocContactBuffer(space), stamp, NULL);
    } else if (stamp - head->next->stamp > space->collisionPersistence) {
        // The tail buffer is available, rotate the ring.
        ContactBufferHeader *tail = head->next;
        space->contactBuffersHead = contactBufferHeaderInit(tail, stamp, tail);
    } else {
        // Allocate a new buffer and push it into the ring.
        ContactBufferHeader *buffer = contactBufferHeaderInit(spaceAllocContactBuffer(space), stamp, head);
        space->contactBuffersHead = head->next = buffer;
    }
}

struct Contact *contactBufferGetArray(Space *space) {
    if (space->contactBuffersHead->numContacts + MAX_CONTACTS_PER_ARBITER > CONTACTS_BUFFER_SIZE) {
        // Contact buffer could overflow on next collision, push a fresh one.
        spacePushFreshContactBuffer(space);
    }

    ContactBufferHeader *head = space->contactBuffersHead;
    return ((ContactBuffer *) head)->contacts + head->numContacts;
}

static void spacePushContacts(Space *space, int count) {
    assertHard(count <= MAX_CONTACTS_PER_ARBITER, VOID_ERR, "Internal Error: Contact buffer overflow!");
    space->contactBuffersHead->numContacts += count;
}

static void spacePopContacts(Space *space, int count) {
    space->contactBuffersHead->numContacts -= count;
}

// Collision detection functions:

static void *spaceArbiterSetTrans(Shape **shapes, Space *space) {
    if (space->pooledArbiters->num == 0) {
        // Arbiter pool is exhausted, make more:
        int count = BUFFER_BYTES / sizeof(Arbiter);
		assertHard(count, VOID_ERR, "Internal Error: Buffer size too small.");

        Arbiter *buffer = (Arbiter *) calloc(1, BUFFER_BYTES);
        arrayPush(space->allocatedBuffers, buffer);

        for (int i = 0; i < count; ++i) {
            arrayPush(space->pooledArbiters, buffer + i);
        }
    }

    return arbiterInit((Arbiter *) arrayPop(space->pooledArbiters), shapes[0], shapes[1]);
}

static inline bool queryReject(Shape *a, Shape *b) {
    return (
        // BBoxes must overlap.
        !BBIntersects(a->bb, b->bb)
        // Don't collide shapes attached to the same body.
        || a->body == b->body
    );

    // NOTE: Currently there are no shape filters and constraints.
}

CollisionID spaceCollideShapes(Shape *a, Shape *b, CollisionID id, Space *space) {
    // Reject any of the simple cases.
    if (queryReject(a, b)) return id;

    // Narrow-phase collision detection.
    struct CollisionInfo info = collide(a, b, id, contactBufferGetArray(space));

    if (info.count == 0) return info.id; // Shapes are not colliding.
    spacePushContacts(space, info.count);

    // Get an arbiter from space->arbiterSet for the two shapes.
    // This is where the persistent contact magic comes from.
    const Shape *shape_pair[] = {info.a, info.b};
    HashValue arbHashID = HASH_PAIR((HashValue) info.a, (HashValue) info.b);
    Arbiter *arb = (Arbiter *) hashSetInsert(space->cachedArbiters, arbHashID, shape_pair, (HashSetTransFunc) spaceArbiterSetTrans, space);
    arbiterUpdate(arb, &info, space);

    CollisionHandler *handler = arb->handler;

    // Call the begin function first if it's the first step.
    if (arb->state == ARBITER_STATE_FIRST_COLLISION && !handler->beginFunc(arb, space, handler->userData)) {
        arbiterIgnore(arb); // permanently ignore the collision until separation
    }

    if (
        // Ignore the arbiter if it has been flagged.
        (arb->state != ARBITER_STATE_IGNORE) &&
        // Call preSolve.
        handler->preSolveFunc(arb, space, handler->userData) &&
        // Check (again) in case the presolve() callback called arbiterIgnored().
        arb->state != ARBITER_STATE_IGNORE &&
        // NOTE: no sensors at the moment.
        // Don't process collisions between two infinite mass bodies.
        !(a->body->mass == INFINITY && b->body->mass == INFINITY)
    ) {
        arrayPush(space->arbiters, arb);
    } else {
        spacePopContacts(space, info.count);

        arb->contacts = NULL;
        arb->count = 0;

        // Normally arbiters are set as used after calling the post-solve callback.
        // However, post-solve() callbacks are not called for sensors or arbiters rejected from pre-solve.
        if (arb->state != ARBITER_STATE_IGNORE) arb->state = ARBITER_STATE_NORMAL;
    }

    // Timestamp the arbiter so we know it was used recently.
    arb->stamp = space->stamp;
    return info.id;
}

// Hashset filter func to throw away old arbiters.
bool spaceArbiterSetFilter(Arbiter *arb, Space *space) {
    Timestamp ticks = space->stamp - arb->stamp;

    Body *a = arb->bodyA, *b = arb->bodyB;

    if ((bodyGetType(a) == STATIC_BODY && bodyGetType(b) == STATIC_BODY)) {
        return true;
    }

    // Arbiter was used last frame, but not this one.
    if (ticks >= 1 && arb->state != ARBITER_STATE_CACHED) {
        arb->state = ARBITER_STATE_CACHED;
        CollisionHandler *handler = arb->handler;
        handler->separateFunc(arb, space, handler->userData);
    }

    if (ticks >= space->collisionPersistence) {
        arb->contacts = NULL;
        arb->count = 0;

        arrayPush(space->pooledArbiters, arb);
        return false;
    }

    return true;
}

// Locking functions.

void spaceLock(Space *space) {
    ++space->locked;
}

void spaceUnlock(Space *space, bool runPostStep) {
    --space->locked;
	assertHard(space->locked >= 0, VOID_ERR, "Internal Error: Space lock underflow.");

    if (space->locked == 0 && runPostStep && !space->skipPostStep) {
        space->skipPostStep = true;

        Array *arr = space->postStepCallbacks;
        for (int i = 0; i < arr->num; ++i) {
            PostStepCallback *callback = (PostStepCallback *) arr->arr[i];
            PostStepFunc func = callback->func;

            // NOTE: Creator's note:
            // Mark func as NULL in case calling it calls SpaceRunPostStepCallbacks() again.
            // TODO: need more tests around this case I think
            callback->func = NULL;
            if (func) func(space, callback->key, callback->data);

            arr->arr[i] = NULL;
            free(callback);
        }

        arr->num = 0;
        space->skipPostStep = false;
    }
}

// Important functions:

void shapeUpdateFunc(Shape *shape, void *unused) {
    shapeCacheBB(shape);
}

void spaceStep(Space *space, double dt) {

    // Do not step if the timestep is 0.
    if (dt == 0.0f) return;

    space->stamp++;

    double prev_dt = space->curr_dt;
    space->curr_dt = dt;

    Array *bodies = space->dynamicBodies;
    Array *arbiters = space->arbiters;

    // Reset and empty arbiter lists.
    for (int i = 0; i < arbiters->num; ++i) {
        Arbiter *arb = (Arbiter *) arbiters->arr[i];
        arb->state = ARBITER_STATE_NORMAL;
        arbiterUnthread(arb);
    }

    spaceLock(space); {
        // Integrate positions.
        for (int i = 0; i < bodies->num; ++i) {
            Body *body = (Body *) bodies->arr[i];
            body->positionFunc(body, dt);
        }

        // Find colliding pairs.
        spacePushFreshContactBuffer(space);
        spatialIndexEach(space->dynamicShapes, (SpatialIndexIteratorFunc) shapeUpdateFunc, NULL);
        spatialIndexReindexQuery(space->dynamicShapes, (SpatialIndexQueryFunc) spaceCollideShapes, space);
    } spaceUnlock(space, false);

    // NOTE: spaceProcessComponents have not been implemented. While the library // seems to work without it, it may be important later on, so look into it.

    spaceLock(space); {
        // Clear out old cached arbiters and call separate callbacks.
        hashSetFilter(space->cachedArbiters, (HashSetFilterFunc) spaceArbiterSetFilter, space);

        // Prestep arbiters.
        double slop = space->collisionSlop;
        double biasCoef = 1.0f - pow(space->collisionBias, dt);
        for (int i = 0; i < arbiters->num; ++i) {
            arbiterPreStep((Arbiter *) arbiters->arr[i], dt, slop, biasCoef);
        }

        // Integrate velocities.
        Vector gravity = space->gravity;
        for (int i = 0; i < bodies->num; ++i) {
            Body *body = (Body *) bodies->arr[i];
            body->velocityFunc(body, gravity, dt);
        }

        // Apply cached impulses.
        double dt_coef = (prev_dt == 0.0f ? 0.0f : dt / prev_dt);
        for (int i = 0; i < arbiters->num; ++i) {
            arbiterApplyCachedImpulse((Arbiter *) arbiters->arr[i], dt_coef);
        }

        // Run the impulse solver.
        for (int i = 0; i < space->iterations; ++i) {
            for (int j = 0; j < arbiters->num; ++j) {
                arbiterApplyImpulse((Arbiter *) arbiters->arr[j]);
            }
        }

        // Run post-solve callbacks.
        for (int i = 0; i < arbiters->num; ++i) {
            Arbiter *arb = (Arbiter *) arbiters->arr[i];

            CollisionHandler *handler = arb->handler;
            handler->postSolveFunc(arb, space, handler->userData);
        }
    } spaceUnlock(space, true);
}
