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

 // HASH SET PRIVATE DATA //

#ifndef HASHSET_PRIVATE_H
#define HASHSET_PRIVATE_H

#include "../Chipmunk_types.h"

#define HASH_COEF (3344921057ul)
#define HASH_PAIR(A, B) ((HashValue)(A) * HASH_COEF ^ (HashValue)(B) * HASH_COEF)

// HASH SETS

//typedef struct HashSetBin HashSetBin;
//typedef struct HashSet HashSet;
typedef bool (*HashSetEqlFunc)(void *ptr, void *elt);
typedef void *(*HashSetTransFunc)(void *ptr, void *data);

typedef struct HashSetBin {
    void *elt;
    HashValue hash;
    struct HashSetBin *next;
} HashSetBin;

typedef struct HashSet {
    unsigned int entries;
    unsigned int size;

    HashSetEqlFunc eql;
    void *defaultVal;

    HashSetBin **table;
    HashSetBin *pooledBins;

    Array *allocatedBuffers;
} HashSet;

HashSet *hashSetNew(int size, HashSetEqlFunc eqlFunc);
void hashSetSetDefaultValue(HashSet *set, void *defaultVal);

void hashSetFree(HashSet *set);

int hashSetCount(HashSet *set);
void *hashSetInsert(HashSet *set, HashValue hash, void *ptr, HashSetTransFunc trans, void *data);
void *hashSetRemove(HashSet *set, HashValue hash, void *ptr);
void *hashSetFind(HashSet *set, HashValue hash, void *ptr);

typedef void (*HashSetIteratorFunc)(void *elt, void *data);
void hashSetEach(HashSet *set, HashSetIteratorFunc func, void *data);

typedef bool (*HashSetFilterFunc)(void *elt, void *data);
void hashSetFilter(HashSet *set, HashSetFilterFunc func, void *data);

#endif
