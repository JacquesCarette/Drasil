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

 // SPATIAL_INDEX_C - SPATIAL INDEX MODULE //

#include "../include/Chipmunk.h"

SpatialIndex *spatialIndexInit(SpatialIndex *index, SpatialIndexClass *klass, SpatialIndexBBFunc bbfunc, SpatialIndex *staticIndex) {
    index->klass = klass;
    index->bbfunc = bbfunc;
    index->staticIndex = staticIndex;

    if (staticIndex) {
        assertHard(!staticIndex->dynamicIndex, PTR_ERR, "This static index is already associated with a dynamic index.");
        staticIndex->dynamicIndex = index;
    }

    return index;
}

void spatialIndexFree(SpatialIndex *index) {
    if (index) {
        spatialIndexDestroy(index);
        free(index);
    }
}

typedef struct DynamicToStaticContext {
    SpatialIndexBBFunc bbfunc;
    SpatialIndex *staticIndex;
    SpatialIndexQueryFunc queryFunc;
    void *data;
} DynamicToStaticContext;

static void dynamicToStaticIter(void *obj, DynamicToStaticContext *context) {
    spatialIndexQuery(context->staticIndex, obj, context->bbfunc(obj), context->queryFunc, context->data);
}

void spatialIndexCollideStatic(SpatialIndex *dynamicIndex, SpatialIndex *staticIndex, SpatialIndexQueryFunc func, void *data) {
    if (staticIndex && spatialIndexCount(staticIndex) > 0) {
        DynamicToStaticContext context = {
            dynamicIndex->bbfunc,
            staticIndex,
            func,
            data
        };
        spatialIndexEach(dynamicIndex, (SpatialIndexIteratorFunc) dynamicToStaticIter, &context);
    }
}
