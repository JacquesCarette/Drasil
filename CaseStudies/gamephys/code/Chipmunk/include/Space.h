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

 // SPACE_H - SPACE MODULE //

#ifndef SPACE_H
#define SPACE_H

// Collision event function callback types.
typedef bool (*CollisionBeginFunc)(Arbiter *arb, Space *space, void *data);
typedef bool (*CollisionPreSolveFunc)(Arbiter *arb, Space *space, void *data);
typedef void (*CollisionPostSolveFunc)(Arbiter *arb, Space *space, void *data);
typedef void (*CollisionSeparateFunc)(Arbiter *arb, Space *space, void *data);

// This structure holds function callback pointers for collision handling
// Collision handlers have a pair of types; when a collision occurs between
// two shapes that have these types, the collision handler functions are
// triggered.
struct CollisionHandler {
    // Collision type identifier of the first shape that this handler
    // recognizes.
    // In the handler callback, the shape with this type is the first argument.1
    const CollisionType typeA;
    // Collision type identifier of the second shape.
    // In the handler callback, the shape with this type is the second argument.
    const CollisionType typeB;
    // This function is called when the two shapes with types that match this
    // handler begin colliding.
    CollisionBeginFunc beginFunc;
    // This function is called during each collision step, before the solver is
    // runs, so that you can affect the collision's outcome.
    CollisionPreSolveFunc preSolveFunc;
    // This function is called during each collision step, after the solver
    // runs, so you can read back info about the collision to trigger in-game
    // events
    CollisionPostSolveFunc postSolveFunc;
    // This function is called when the two colliding shapes stop colliding.
    CollisionSeparateFunc separateFunc;
    // This is a user-definable context pointer that is passed to all collision
    // handler functions.
    DataPointer userData;
};


 typedef void (*PostStepFunc)(Space *space, void *key, void *data);

// Memory allocators and constructor for spaces.
Space *spaceAlloc(void);
Space *spaceInit(Space *space);
Space *spaceNew(void);

// Memory freeing functions for spaces.
void spaceDestroy(Space *space);
void spaceFree(Space *space);

// Obtains and sets the number of iterations to use in the collision solver
// to solve contacts.
int spaceGetIterations(const Space *space);
void spaceSetIterations(Space *space, int iterations);

// Obtains and sets the gravitational force in this space.
Vector spaceGetGravity(const Space *space);
void spaceSetGravity(Space *space, Vector gravity);

// Obtains and sets the "amount of encouraged penetration between colliding
// shapes."
double spaceGetCollisionSlop(const Space *space);
void spaceSetCollisionSlop(Space *space, double collisionSlop);

// Obtains and sets the collision bias, which determines how fast overlapping
// shapes are pushed apart.
double spaceGetCollisionBias(const Space *space);
void spaceSetCollisionBias(Space *space, double collisionBias);

// Obtains and sets the number of frames that contact information should
// persist. Defaults to 3.
Timestamp spaceGetCollisionPersistence(const Space *space);
void spaceSetCollisionPersistence(Space *space, Timestamp collisionPersistence);

// Returns the most recent time step used with the given space.
double spaceGetCurrentTimeStep(const Space *space);

// Obtains and sets the static body attached to this space.
Body *spaceGetStaticBody(const Space *space);

// Checks if a space is currently locked (cannot be modified).
bool spaceIsLocked(const Space *space);

// Collision handler management functions.
// TODO: Remove if unnecessary.

// Create or return the default collision handler called for all collisions.
CollisionHandler *spaceAddDefaultCollisionHandler(Space *space);
// Create or return the existing collision handler for the specified pair of
// collision types.
CollisionHandler *spaceAddCollisionHandler(Space *space, CollisionType a, CollisionType b);
// Create or return the existing wildcard collision handler for the specified
// type.
CollisionHandler *spaceAddWildcardHandler(Space *space, CollisionType type);

// Body and shape management functions:
// Adds a body or collision shape to the space.
Shape *spaceAddShape(Space *space, Shape *shape);
Body *spaceAddBody(Space *space, Body *body);

// Removes a body or collision shape to the space.
void spaceRemoveShape(Space *space, Shape *shape);
void spaceRemoveBody(Space *space, Body *body);

// Checks if the space contains the specified body or collision shape.
bool spaceContainsShape(Space *space, Shape *shape);
bool spaceContainsBody(Space *space, Body *body);

// Iterator function definitions.
typedef void (*SpaceBodyIteratorFunc)(Body *body, void *data);
typedef void (*SpaceShapeIteratorFunc)(Shape *shape, void *data);

// Iterators for rigid bodies and shapes in the space.
void spaceEachBody(Space *space, SpaceBodyIteratorFunc func, void *data);
void spaceEachShape(Space *space, SpaceShapeIteratorFunc func, void *data);

// Spatial index management functions.
void spaceReindexStatic(Space *space);
void spaceReindexShape(Space *space, Shape *shape);
void spaceReindexShapesForBody(Space *space, Body *body);

// Step the space forward in time by dt.
void spaceStep(Space *space, double dt);

#endif
