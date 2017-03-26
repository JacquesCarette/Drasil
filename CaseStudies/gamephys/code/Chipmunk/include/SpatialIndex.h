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

 // SPATIAL_INDEX_H - SPATIAL INDEX MODULE //

#ifndef SPATIAL_INDEX_H
#define SPATIAL_INDEX_H

// Function type definitions.
typedef BB (*SpatialIndexBBFunc)(void *obj);
typedef void (*SpatialIndexIteratorFunc)(void *obj, void *data);
typedef CollisionID (*SpatialIndexQueryFunc)(void *obj1, void *obj2, CollisionID id, void *data);

typedef struct SpatialIndexClass SpatialIndexClass;
typedef struct SpatialIndex SpatialIndex;

struct SpatialIndex {
    SpatialIndexClass *klass;
    SpatialIndexBBFunc bbfunc;

    SpatialIndex *staticIndex;
    SpatialIndex *dynamicIndex;
};

// Spatial index implementation.

typedef void (*SpatialIndexDestroyImpl)(SpatialIndex *index);

typedef int (*SpatialIndexCountImpl)(SpatialIndex *index);
typedef void (*SpatialIndexEachImpl)(SpatialIndex *index, SpatialIndexIteratorFunc func, void *data);

typedef bool (*SpatialIndexContainsImpl)(SpatialIndex *index, void *object, HashValue hashId);
typedef void (*SpatialIndexInsertImpl)(SpatialIndex *index, void *object, HashValue hashId);
typedef void (*SpatialIndexRemoveImpl)(SpatialIndex *index, void *object, HashValue hashId);

typedef void (*SpatialIndexReindexImpl)(SpatialIndex *index);
typedef void (*SpatialIndexReindexObjectImpl)(SpatialIndex *index, void *object, HashValue hashId);
typedef void (*SpatialIndexReindexQueryImpl)(SpatialIndex *index, SpatialIndexQueryFunc func, void *data);

typedef void (*SpatialIndexQueryImpl)(SpatialIndex *index, void *object, BB bb, SpatialIndexQueryFunc func, void *data);

struct SpatialIndexClass {
    SpatialIndexDestroyImpl destroy;

    SpatialIndexCountImpl count;
    SpatialIndexEachImpl each;

    SpatialIndexContainsImpl contains;
    SpatialIndexInsertImpl insert;
    SpatialIndexRemoveImpl remove;

    SpatialIndexReindexImpl reindex;
    SpatialIndexReindexObjectImpl reindexObject;
    SpatialIndexReindexQueryImpl reindexQuery;

    SpatialIndexQueryImpl query;
};

// Spatial index function headers.
void spatialIndexFree(SpatialIndex *index);
void spatialIndexCollideStatic(SpatialIndex *dynamicIndex, SpatialIndex *staticIndex, SpatialIndexQueryFunc func, void *data);

// Spatial index functions.
static inline void spatialIndexDestroy(SpatialIndex *index) {
    if (index->klass) index->klass->destroy(index);
}

static inline int spatialIndexCount(SpatialIndex *index) {
    return index->klass->count(index);
}

static inline void spatialIndexEach(SpatialIndex *index, SpatialIndexIteratorFunc func, void *data) {
    index->klass->each(index, func, data);
}

static inline bool spatialIndexContains(SpatialIndex *index, void *obj, HashValue hashId) {
    return index->klass->contains(index, obj, hashId);
}

static inline void spatialIndexInsert(SpatialIndex *index, void *obj, HashValue hashId) {
    index->klass->insert(index, obj, hashId);
}

static inline void spatialIndexRemove(SpatialIndex *index, void *obj, HashValue hashId) {
    index->klass->remove(index, obj, hashId);
}

static inline void spatialIndexReindex(SpatialIndex *index) {
    index->klass->reindex(index);
}

static inline void spatialIndexReindexObject(SpatialIndex *index, void *obj, HashValue hashId) {
    index->klass->reindexObject(index, obj, hashId);
}

static inline void spatialIndexQuery(SpatialIndex *index, void *obj, BB bb, SpatialIndexQueryFunc func, void *data) {
    index->klass->query(index, obj, bb, func, data);
}

static inline void spatialIndexReindexQuery(SpatialIndex *index, SpatialIndexQueryFunc func, void *data) {
    index->klass->reindexQuery(index, func, data);
}

#endif
