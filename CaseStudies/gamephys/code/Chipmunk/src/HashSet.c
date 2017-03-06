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

 // HASH_SET_C - HASH SET MODULE //

#include "../include/Chipmunk.h"

// From primes.h
// Used for resizing hash tables.
// Values approximately double.
// http://planetmath.org/encyclopedia/GoodHashTablePrimes.html
static int primes[] = {
	5,
	13,
	23,
	47,
	97,
	193,
	389,
	769,
	1543,
	3079,
	6151,
	12289,
	24593,
	49157,
	98317,
	196613,
	393241,
	786433,
	1572869,
	3145739,
	6291469,
	12582917,
	25165843,
	50331653,
	100663319,
	201326611,
	402653189,
	805306457,
	1610612741,
	0,
};

static inline int
next_prime(int n)
{
	int i = 0;
	while(n > primes[i]){
		i++;
		assertHard(primes[i], INT_ERR, "Tried to resize a hash table to a size greater than 1610612741."); // realistically this should never happen
	}

	return primes[i];
}

struct HashSetBin {
    void *elt;
    HashValue hash;
    struct HashSetBin *next;
};

// Error value for unit testing.
#ifdef UNIT_TEST
    static const struct HashSetBin errorBin = {NULL, UINTPTR_MAX, NULL};
    #define BIN_ERR errorBin
#endif

struct HashSet {
    unsigned int entries;
    unsigned int size;

    HashSetEqlFunc eql;
    void *defaultVal;

    HashSetBin **table;
    HashSetBin *pooledBins;

    Array *allocatedBuffers;
};

HashSet *hashSetNew(int size, HashSetEqlFunc eqlFunc) {
    HashSet *set = (HashSet *) calloc(1, sizeof(HashSet));

    set->size = next_prime(size);
    set->entries = 0;

    set->eql = eqlFunc;
    set->defaultVal = NULL;

    set->table = (HashSetBin **) calloc(set->size, sizeof(HashSetBin *));
    set->pooledBins = NULL;

    set->allocatedBuffers = arrayNew(0);

    return set;
}

void hashSetSetDefaultValue(HashSet *set, void *defaultVal) {
    set->defaultVal = defaultVal;
}

void hashSetFree(HashSet *set) {
    if (set) {
        free(set->table);

        arrayFreeEach(set->allocatedBuffers, free);
        arrayFree(set->allocatedBuffers);

        free(set);
    }
}

static bool setIsFull(HashSet *set) {
    return (set->entries >= set->size);
}

static void hashSetResize(HashSet *set) {
    // Get the next approximate doubled prime.
    unsigned int newSize = next_prime(set->size + 1);

    // Allocate a new table.
    HashSetBin **newTable = (HashSetBin **) calloc(newSize, sizeof(HashSetBin *));

    // Iterate over the chains.
    for (unsigned int i = 0; i < set->size; ++i) {
        // Rehash the bins into the new table.
        HashSetBin *bin = set->table[i];
        while (bin) {
            HashSetBin *next = bin->next;

            HashValue index = bin->hash % newSize;
            bin->next = newTable[index];
            newTable[index] = bin;

            bin = next;
        }
    }

    free(set->table);

    set->table = newTable;
    set->size = newSize;
}

static inline void recycleBin(HashSet *set, HashSetBin *bin) {
    bin->next = set->pooledBins;
    set->pooledBins = bin;
    bin->elt = NULL;
}

static HashSetBin *getUnusedBin(HashSet *set) {
    HashSetBin *bin = set->pooledBins;

    if (bin) {
        set->pooledBins = bin->next;
        return bin;
    } else {
        // Pool is exhausted, make more.
        int count = BUFFER_BYTES / sizeof(HashSetBin);
        assertHard(count, BIN_ERR, "Internal Error: Buffer size is too small.");

        HashSetBin *buffer = (HashSetBin *) calloc(1, BUFFER_BYTES);
        arrayPush(set->allocatedBuffers, buffer);

        // Push all but the first one - return it instead.
        for (int i = 1; i < count; ++i) recycleBin(set, buffer + i);
        return buffer;
    }
}

int hashSetCount(HashSet *set) {
    return set->entries;
}

void *hashSetInsert(HashSet *set, HashValue hash, void *ptr, HashSetTransFunc trans, void *data) {
    HashValue index = hash % set->size;

    // Find the bin with the matching element.
    HashSetBin *bin = set->table[index];
    while (bin && !set->eql(ptr, bin->elt))
        bin = bin->next;

    // Create it if necessary.
    if (!bin) {
        bin = getUnusedBin(set);
        bin->hash = hash;
        bin->elt = (trans ? trans(ptr, data) : data);

        bin->next = set->table[index];
        set->table[index] = bin;

        set->entries++;
        if (setIsFull(set)) hashSetResize(set);
    }

    return bin->elt;
}

void *hashSetRemove(HashSet *set, HashValue hash, void *ptr) {
    HashValue index = hash % set->size;

    HashSetBin **prevptr = &set->table[index];
    HashSetBin *bin = set->table[index];

    // Find the bin.
    while (bin && !set->eql(ptr, bin->elt)) {
        prevptr = &bin->next;
        bin = bin->next;
    }

    // Remove if it exists.
    if (bin) {
        // Update the previous linked list pointer.
        (*prevptr) = bin->next;
        --set->entries;

        void *elt = bin->elt;
        recycleBin(set, bin);

        return elt;
    }

    return NULL;
}

void *hashSetFind(HashSet *set, HashValue hash, void *ptr) {
    HashValue index = hash % set->size;
    HashSetBin *bin = set->table[index];

    while (bin && !set->eql(ptr, bin->elt)) bin = bin->next;

    return (bin ? bin->elt : set->defaultVal);
}

void hashSetEach(HashSet *set, HashSetIteratorFunc func, void *data) {
    for (unsigned int i = 0; i < set->size; ++i) {
        HashSetBin *bin = set->table[i];
        while (bin) {
            HashSetBin *next = bin->next;
            func(bin->elt, data);
            bin = next;
        }
    }
}

void hashSetFilter(HashSet *set, HashSetFilterFunc func, void *data) {
    for (unsigned int i = 0; i < set->size; ++i) {
        // similar to hashSetRemove().
        HashSetBin **prevptr = &set->table[i];
        HashSetBin *bin = set->table[i];

        while (bin) {
            HashSetBin *next = bin->next;

            if (func(bin->elt, data)) {
                prevptr = &bin->next;
            } else {
                (*prevptr) = next;
                --set->entries;
                recycleBin(set, bin);
            }

            bin = next;
        }
    }
}
