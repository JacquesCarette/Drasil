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

 // CHIPMUNK INTERNAL TYPE DEFINITIONS & MISC FUNCTIONS //

#ifndef CHIPMUNK_TYPES_H
#define CHIPMUNK_TYPES_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include <float.h>
#include <math.h>
#include <limits.h>

#ifdef WIN32
    #include <malloc.h>
#else
    #include <alloca.h>
#endif

// math.h does not define M_PI by default, so it is defined here
// for compatibility reasons.
#ifndef M_PI
    #define M_PI 3.14159265358979323846
#endif

void message(const char *condition, const char *file, int line, int isError, int isHardError, const char *message, ...);

// Assertion macros.
// NOTE: For normal use, assertSoft and assertHard will abort the program.
// However, for unit tests, all three macros will return a specific sentinel
// value depending on the return type of the function where the assertion is
// used.

#ifdef UNIT_TEST
    // Unit test standard error definitions.
    #define VOID_ERR
    #define PTR_ERR NULL
    #define INT_ERR INT_MIN
    #define DBL_ERR DBL_MIN

    #define assertSoft(__condition__, __retval__, ...) if(!(__condition__)){message(#__condition__, __FILE__, __LINE__, 1, 0, __VA_ARGS__); return __retval__;}

    #define assertWarn(__condition__, __retval__, ...) if(!(__condition__)){message(#__condition__, __FILE__, __LINE__, 0, 0, __VA_ARGS__); return __retval__;}

    #define assertHard(__condition__, __retval__, ...) if(!(__condition__)){message(#__condition__, __FILE__, __LINE__, 1, 1, __VA_ARGS__); return __retval__;}
#else
    #define assertSoft(__condition__, __retval__, ...) if(!(__condition__)){message(#__condition__, __FILE__, __LINE__, 1, 0, __VA_ARGS__); abort();}

    #define assertWarn(__condition__, __retval__, ...) if(!(__condition__)){message(#__condition__, __FILE__, __LINE__, 0, 0, __VA_ARGS__);}

    // Used when the program will definitely crash, and the exception is inexpensive to detect.
    #define assertHard(__condition__, __retval__, ...) if(!(__condition__)){message(#__condition__, __FILE__, __LINE__, 1, 1, __VA_ARGS__); abort();}
#endif

// Allocated size for various Chipmunk buffers.
#ifndef BUFFER_BYTES
    #define BUFFER_BYTES (32 * 1024)
#endif

// Hash value type.
typedef uintptr_t HashValue;
// Data pointer type.
typedef void *DataPointer;
// Type used for collision types.
typedef uintptr_t CollisionType;
// Type used for various timestamps in Chipmunk.
typedef unsigned int Timestamp;
// Type used internally to cache colliding object info for collideShapes().
// Should be at least 32 bits.
typedef uint32_t CollisionID;

#ifndef WILDCARD_COLLISION_TYPE
// CollisionType value internally reserved for hashing wildcard handlers.
    #define WILDCARD_COLLISION_TYPE (~(CollisionType) 0)
#endif

// Simple numerical functions (from chipmunk_types.h):
static inline double fclamp(double f, double min, double max) {
    return fmin(fmax(f, min), max);
}

static inline double fclamp01(double f) {
    return fclamp(f, 0, 1);
}

static inline double flerp(double f1, double f2, double t) {
    return f1 * (1.0f - t) + f2 * t;
}

static inline double flerpconst(double f1, double f2, double d) {
    return f1 + fclamp(f2 - f1, -d, d);
}

// Type definitions.
typedef struct Array Array;
typedef struct HashSet HashSet;

typedef struct Body Body;

typedef struct Shape Shape;
typedef struct CircleShape CircleShape;
typedef struct SegmentShape SegmentShape;
typedef struct PolyShape PolyShape;

typedef struct CollisionHandler CollisionHandler;
typedef struct ContactPointSet ContactPointSet;
typedef struct Arbiter Arbiter;

typedef struct Space Space;

// Utilities header files.
#include "Vector.h"
#include "BB.h"
#include "Transform.h"
#include "SpatialIndex.h"

// Main objects header files.
#include "Arbiter.h"
#include "Body.h"
#include "Shape.h"
#include "Space.h"

void loopIndices(const Vector *verts, int count, int *start, int *end);
int convexHull(int count, const Vector *verts, Vector *result, int *first, double tol);

#endif
