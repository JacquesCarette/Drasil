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

 // SPACE PRIVATE DATA //

 #ifndef SPACE_PRIVATE_H
 #define SPACE_PRIVATE_H

 #include "HashSet_private.h"
 #include "Array_private.h"
 #include "Arbiter_private.h"
 #include "../Chipmunk_types.h"

 typedef struct ContactBufferHeader ContactBufferHeader;
 // typedef void (*SpaceArbiterApplyImpulseFunc)(Arbiter *arb);

 struct Space {
     int iterations;

     Vector gravity;

     double collisionSlop;
     double collisionBias;
     Timestamp collisionPersistence;

     Timestamp stamp;
     double curr_dt;

     Array *dynamicBodies;
     Array *staticBodies;

     HashValue shapeIDCounter;
     SpatialIndex *staticShapes;
     SpatialIndex *dynamicShapes;

     Array *arbiters;
     ContactBufferHeader *contactBuffersHead;
     HashSet *cachedArbiters;
     Array *pooledArbiters;

     Array *allocatedBuffers;
     unsigned int locked;

     bool usesWildcards;
     HashSet *collisionHandlers;
     CollisionHandler defaultHandler;

     bool skipPostStep;
     Array *postStepCallbacks;

     Body *staticBody;
     Body _staticBody;
 };

#define assertSpaceUnlocked(space) \
    assertHard(!space->locked, NULL, \
    "This operation cannot be done safely during a call to spaceStep() or during a query. " \
    "Put these calls into a post-step callback." \
    );

 // Sets the static body attached to this space.
 void spaceSetStaticBody(Space *space, Body *body);

 extern CollisionHandler collisionHandlerDoNothing;

 void spacePushFreshContactBuffer(Space *space);
 struct Contact *contactBufferGetArray(Space *space);

 typedef struct PostStepCallback {
     PostStepFunc func;
     void *key;
     void *data;
 } PostStepCallback;

 bool spaceArbiterSetFilter(Arbiter *arb, Space *space);
 void spaceFilterArbiters(Space *space, Body *body, Shape *filter);

 void spaceLock(Space *space);
 void spaceUnlock(Space *space, bool runPostStep);

 static inline Array *spaceArrayForBodyType(Space *space, BodyType type) {
     return (type == STATIC_BODY ? space->staticBodies : space->dynamicBodies);
 }

 void shapeUpdateFunc(Shape *shape, void *unused);
 CollisionID spaceCollideShapes(Shape *a, Shape *b, CollisionID id, Space *space);

#endif
