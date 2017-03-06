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

// CHIPMUNK LIBRARY //

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "../include/Chipmunk.h"

// Error message printer - for assertions.
void message(const char *condition, const char *file, int line, int isError, int isHardError, const char *message, ...) {
    fprintf(stderr, (isError ? "Aborting due to Chipmunk error: " : "Chipmunk warning: "));

    va_list vargs;
    va_start(vargs, message); {
        vfprintf(stderr, message, vargs);
        fprintf(stderr, "\n");
    } va_end(vargs);

    fprintf(stderr, "\tFailed condition: %s\n", condition);
    fprintf(stderr, "\tSource:%s:%d\n", file, line);
}

// Quick Hull functions:

void loopIndices(const Vector *verts, int count, int *start, int *end) {
    (*start) = (*end) = 0;
    Vector fmin = verts[0];
    Vector fmax = fmin;

    for (int i = 1; i < count; ++i) {
        Vector v = verts[i];
        if (v.x < fmin.x || (v.x == fmin.x && v.y < fmin.y)) {
            fmin = v;
            (*start) = i;
        } else if (v.x > fmax.x || (v.x == fmax.x && v.y > fmax.y)) {
            fmax = v;
            (*end) = i;
        }
    }
}

#define SWAP(__A__, __B__) {Vector __TMP__ = __A__; __A__ = __B__; __B__ = __TMP__;}

static int QHullPartition(Vector *verts, int count, Vector a, Vector b, double tol) {
    if (count == 0) return 0;

    double fmax = 0;
    int pivot = 0;

    Vector delta = vectSub(b, a);
    double valueTol = tol * vectLength(delta);

    int head = 0;
    for (int tail = count - 1; head <= tail;) {
        double value = vectCross(vectSub(verts[head], a), delta);
        if (value > valueTol) {
            if (value > fmax) {
                fmax = value;
                pivot = head;
            }
            ++head;
        } else {
            SWAP(verts[head], verts[tail]);
            --tail;
        }
    }

    // Move the new pivot to the front if not already there.
    if (pivot != 0) SWAP(verts[0], verts[pivot]);
    return head;
}

static int QHullReduce(double tol, Vector *verts, int count, Vector a, Vector pivot, Vector b, Vector *result) {
    if (count < 0) {
        return 0;
    } else if (count == 0) {
        result[0] = pivot;
        return 1;
    } else {
        int leftCount = QHullPartition(verts, count, a, pivot, tol);
        int index = QHullReduce(tol, verts + 1, leftCount - 1, a, verts[0], pivot, result);

        result[index++] = pivot;

        int rightCount = QHullPartition(verts + leftCount, count - leftCount, pivot, b, tol);
        return index + QHullReduce(tol, verts + leftCount + 1, rightCount - 1, pivot, verts[leftCount], b, result + index);
    }
}

int convexHull(int count, const Vector *verts, Vector *result, int *first, double tol) {
    if (verts != result) {
        // Copy the line vertices into the empty part of the result polyline to use as a scratch buffer.
        memcpy(result, verts, count * sizeof(Vector));
    }

    // Degenerate case, all points are the same.
    int start, end;
    loopIndices(verts, count, &start, &end);
    if (start == end) {
        if (first) (*first) = 0;
        return 1;
    }

    SWAP(result[0], result[start]);
    SWAP(result[1], result[end == 0 ? start : end]);

    Vector a = result[0];
    Vector b = result[1];

    if (first) (*first) = start;
    return QHullReduce(tol, result + 2, count - 2, a, b, a, result + 1);
}
