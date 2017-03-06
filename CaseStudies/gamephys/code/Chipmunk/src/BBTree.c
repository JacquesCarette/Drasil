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

 // BBTREE_C - BOUNDING BOX (AABB) TREES //

#include <stdlib.h>
#include <stdio.h>

#include "../include/Chipmunk.h"

static inline SpatialIndexClass *Klass();

typedef struct Node Node;
typedef struct Pair Pair;

struct BBTree {
    SpatialIndex spatialIndex;
    BBTreeVelocityFunc velocityFunc;

    HashSet *leaves;
    Node *root;

    Node *pooledNodes;
    Pair *pooledPairs;
    Array *allocatedBuffers;

    Timestamp stamp;
};

struct Node {
    void *obj;
    BB bb;
    Node *parent;

    union {
        // Internal nodes.
        struct { Node *a, *b; } children;

        // Leaves.
        struct {
            Timestamp stamp;
            Pair *pairs;
        } leaf;
    } node;
};

// Creator's NOTE: Can't use anonymous unions and still get good x-compiler
// compatibility.
#define A node.children.a
#define B node.children.b
#define STAMP node.leaf.stamp
#define PAIRS node.leaf.pairs

typedef struct Thread {
    Pair *prev;
    Node *leaf;
    Pair *next;
} Thread;

struct Pair {
    Thread a, b;
    CollisionID id;
};

// Error values for unit testing.
#ifdef UNIT_TEST
    static const Node errorNode = {NULL, BB_ERR, NULL};
    static const Pair errorPair = {{NULL, NULL, NULL}, {NULL, NULL, NULL}, UINT32_MAX};
    #define NODE_ERR errorNode
    #define PAIR_ERR errorPair
#endif

// Miscellaneous functions.

static inline BB getBB(BBTree *tree, void *obj) {
    BB bb = tree->spatialIndex.bbfunc(obj);

    BBTreeVelocityFunc velocityFunc = tree->velocityFunc;
    if (velocityFunc) {
        double coef = 0.1f;
        double x = (bb.right - bb.left) * coef;
        double y = (bb.top - bb.bottom) * coef;

        Vector v = vectMult(velocityFunc(obj), 0.1f);

        return BBNew(bb.left + fmin(-x, v.x), bb.bottom + fmin(-y, v.y), bb.right + fmax(x, v.x), bb.top + fmax(y, v.y));
    } else {
        return bb;
    }
}

static inline BBTree *getTree(SpatialIndex *index) {
    return (index && index->klass == Klass() ? (BBTree *) index : NULL);
}

static inline Node *getRootIfTree(SpatialIndex *index) {
    return (index && index->klass == Klass() ? ((BBTree *) index)->root : NULL);
}

static inline BBTree *getMasterTree(BBTree *tree) {
    BBTree *dynamicTree = getTree(tree->spatialIndex.dynamicIndex);
    return (dynamicTree ? dynamicTree : tree);
}

static inline void incrementStamp(BBTree *tree) {
    BBTree *dynamicTree = getTree(tree->spatialIndex.dynamicIndex);
    if (dynamicTree) {
        dynamicTree->stamp++;
    } else {
        tree->stamp++;
    }
}

// Pair/thread functions

static void pairRecycle(BBTree *tree, Pair *pair) {
    // Share the pool of the master tree
    tree = getMasterTree(tree);

    pair->a.next = tree->pooledPairs;
    tree->pooledPairs = pair;
}

static Pair *pairFromPool(BBTree *tree) {
    // Share the pool of the master tree
    tree = getMasterTree(tree);

    Pair *pair = tree->pooledPairs;

    if (pair) {
        tree->pooledPairs = pair->a.next;
        return pair;
    } else {
        // Pool is exhausted, make more
        int count = BUFFER_BYTES / sizeof(Pair);
        assertHard(count, PAIR_ERR, "Internal Error: Buffer size is too small.");

        Pair *buffer = (Pair *) calloc(1, BUFFER_BYTES);
        arrayPush(tree->allocatedBuffers, buffer);

        // push all but the first one, return the first instead
        for (int i = 1; i < count; ++i) {
            pairRecycle(tree, buffer + i);
        }

        return buffer;
    }
}

static inline void threadUnlink(Thread thread) {
    Pair *next = thread.next;
    Pair *prev = thread.prev;

    if (next) {
        if (next->a.leaf == thread.leaf) {
            next->a.prev = prev;
        } else {
            next->b.prev = prev;
        }
    }

    if (prev) {
        if (prev->a.leaf == thread.leaf) {
            prev->a.next = next;
        } else {
            prev->b.next = next;
        }
    } else {
        thread.leaf->PAIRS = next;
    }
}

static void pairsClear(Node *leaf, BBTree *tree) {
    Pair *pair = leaf->PAIRS;
    leaf->PAIRS = NULL;

    while (pair) {
        if (pair->a.leaf == leaf) {
            Pair *next = pair->a.next;
            threadUnlink(pair->b);
            pairRecycle(tree, pair);
            pair = next;
        } else {
            Pair *next = pair->b.next;
            threadUnlink(pair->a);
            pairRecycle(tree, pair);
            pair = next;
        }
    }
}

static void pairInsert(Node *a, Node *b, BBTree *tree) {
    Pair *nextA = a->PAIRS, *nextB = b->PAIRS;
    Pair *pair = pairFromPool(tree);
    Pair temp = {{NULL, a, nextA}, {NULL, b, nextB}, 0};

    a->PAIRS = b->PAIRS = pair;
    *pair = temp;

    if (nextA) {
        if (nextA->a.leaf == a) {
            nextA->a.prev = pair;
        } else {
            nextA->b.prev = pair;
        }
    }

    if (nextB) {
        if (nextB->a.leaf == b) {
            nextB->a.prev = pair;
        } else {
            nextB->b.prev = pair;
        }
    }
}

// Node functions

static void nodeRecycle(BBTree *tree, Node *node) {
    node->parent = tree->pooledNodes;
    tree->pooledNodes = node;
}

static Node *nodeFromPool(BBTree *tree) {
    Node *node = tree->pooledNodes;

    if (node) {
        tree->pooledNodes = node->parent;
        return node;
    } else {
        // Pool is exhausted, make more
        int count = BUFFER_BYTES / sizeof(Node);
        assertHard(count, NODE_ERR, "Internal Error: Buffer size is too small.");

        Node *buffer = (Node *) calloc(1, BUFFER_BYTES);
        arrayPush(tree->allocatedBuffers, buffer);

        // push all but the first one, return the first instead
        for (int i = 1; i < count; ++i) {
            nodeRecycle(tree, buffer + i);
        }

        return buffer;
    }
}

static inline void nodeSetA(Node *node, Node *value) {
    node->A = value;
    value->parent = node;
}

static inline void nodeSetB(Node *node, Node *value) {
    node->B = value;
    value->parent = node;
}

static Node *nodeNew(BBTree *tree, Node *a, Node *b) {
    Node *node = nodeFromPool(tree);

    node->obj = NULL;
    node->bb = BBMerge(a->bb, b->bb);
    node->parent = NULL;

    nodeSetA(node, a);
    nodeSetB(node, b);

    return node;
}

static inline bool nodeIsLeaf(Node *node) {
    return (node->obj != NULL);
}

static inline Node *nodeOther(Node *node, Node *child) {
    return (node->A == child ? node->B : node->A);
}

static inline void nodeReplaceChild(Node *parent, Node *child, Node *value, BBTree *tree) {
    assertSoft(!nodeIsLeaf(parent), VOID_ERR, "Internal Error: Cannot replace child of a leaf.");
    assertSoft(child == parent->A || child == parent->B, VOID_ERR, "Internal Error: Node is not a child of parent.");

    if (parent->A == child) {
        nodeRecycle(tree, parent->A);
        nodeSetA(parent, value);
    } else {
        nodeRecycle(tree, parent->B);
        nodeSetB(parent, value);
    }

    for (Node *node = parent; node; node = node->parent) {
        node->bb = BBMerge(node->A->bb, node->B->bb);
    }
}

// Subtree functions

static inline double BBProximity(BB a, BB b) {
    return fabs(a.left + a.right - b.left - b.right) + fabs(a.bottom + a.top - b.bottom - b.top);
}

static Node *subtreeInsert(Node *subtree, Node *leaf, BBTree *tree) {
    if (subtree == NULL) {
        return leaf;
    } else if (nodeIsLeaf(subtree)) {
        return nodeNew(tree, leaf, subtree);
    } else {
        double costA = BBArea(subtree->B->bb) + BBMergedArea(subtree->A->bb, leaf->bb);
        double costB = BBArea(subtree->A->bb) + BBMergedArea(subtree->B->bb, leaf->bb);

        if (costA == costB) {
            costA = BBProximity(subtree->A->bb, leaf->bb);
            costB = BBProximity(subtree->B->bb, leaf->bb);
        }

        if (costB < costA) {
            nodeSetB(subtree, subtreeInsert(subtree->B, leaf, tree));
        } else {
            nodeSetA(subtree, subtreeInsert(subtree->A, leaf, tree));
        }

        subtree->bb = BBMerge(subtree->bb, leaf->bb);
        return subtree;
    }
}

static inline Node *subtreeRemove(Node *subtree, Node *leaf, BBTree *tree) {
    if (leaf == subtree) {
        return NULL;
    } else {
        Node *parent = leaf->parent;
        if (parent == subtree) {
            Node *other = nodeOther(subtree, leaf);
            other->parent = subtree->parent;
            nodeRecycle(tree, subtree);
            return other;
        } else {
            nodeReplaceChild(parent->parent, parent, nodeOther(parent, leaf), tree);
            return subtree;
        }
    }
}

// Marking functions

typedef struct MarkContext {
    BBTree *tree;
    Node *staticRoot;
    SpatialIndexQueryFunc func;
    void *data;
} MarkContext;

static void markLeafQuery(Node *subtree, Node *leaf, bool left, MarkContext *context) {
    if (BBIntersects(leaf->bb, subtree->bb)) {
        if (nodeIsLeaf(subtree)) {
            if (left) {
                pairInsert(leaf, subtree, context->tree);
            } else {
                if (subtree->STAMP < leaf->STAMP) pairInsert(subtree, leaf, context->tree);
                context->func(leaf->obj, subtree->obj, 0, context->data);
            }
        } else {
            markLeafQuery(subtree->A, leaf, left, context);
            markLeafQuery(subtree->B, leaf, left, context);
        }
    }
}

static void markLeaf(Node *leaf, MarkContext *context) {
    BBTree *tree = context->tree;
    if (leaf->STAMP == getMasterTree(tree)->stamp) {
        Node *staticRoot = context->staticRoot;
        if (staticRoot) markLeafQuery(staticRoot, leaf, false, context);

        for (Node *node = leaf; node->parent; node = node->parent) {
            if (node == node->parent->A) {
                markLeafQuery(node->parent->B, leaf, true, context);
            } else {
                markLeafQuery(node->parent->A, leaf, false, context);
            }
        }
    } else {
        Pair *pair = leaf->PAIRS;
        while (pair) {
            if (leaf == pair->b.leaf) {
                pair->id = context->func(pair->a.leaf->obj, leaf->obj, pair->id, context->data);
                pair = pair->b.next;
            } else {
                pair = pair->a.next;
            }
        }
    }
}

static void markSubtree(Node *subtree, MarkContext *context) {
    if (nodeIsLeaf(subtree)) {
        markLeaf(subtree, context);
    } else {
        markSubtree(subtree->A, context);
        markSubtree(subtree->B, context);
    }
}

// Leaf functions

static Node *leafNew(BBTree *tree, void *obj, BB bb) {
    Node *node = nodeFromPool(tree);
    node->obj = obj;
    node->bb = getBB(tree, obj);

    node->parent = NULL;
    node->STAMP = 0;
    node->PAIRS = NULL;

    return node;
}

static bool leafUpdate(Node *leaf, BBTree *tree) {
    Node *root = tree->root;
    BB bb = tree->spatialIndex.bbfunc(leaf->obj);

    if (BBContainsBB(leaf->bb, bb)) {
        leaf->bb = getBB(tree, leaf->obj);

        root = subtreeRemove(root, leaf, tree);
        tree->root = subtreeInsert(root, leaf, tree);

        pairsClear(leaf, tree);
        leaf->STAMP = getMasterTree(tree)->stamp;

        return true;
    } else {
        return false;
    }
}

static CollisionID voidQueryFunc(void *obj1, void *obj2, CollisionID id, void *data) {return id;}

static void leafAddPairs(Node *leaf, BBTree *tree) {
    SpatialIndex *dynamicIndex = tree->spatialIndex.dynamicIndex;
    if (dynamicIndex) {
        Node *dynamicRoot = getRootIfTree(dynamicIndex);
        if (dynamicRoot) {
            BBTree *dynamicTree = getTree(dynamicIndex);
            MarkContext context = {dynamicTree, NULL, NULL, NULL};
            markLeafQuery(dynamicRoot, leaf, true, &context);
        }
    } else {
        Node *staticRoot = getRootIfTree(tree->spatialIndex.staticIndex);
        MarkContext context = {tree, staticRoot, voidQueryFunc, NULL};
        markLeaf(leaf, &context);
    }
}

// Memory management functions

BBTree *BBTreeAlloc(void) {
    return (BBTree *) calloc(1, sizeof(BBTree));
}

static bool leafSetEql(void *obj, Node *node) {
    return (obj == node->obj);
}

static void *leafSetTrans(void *obj, BBTree *tree) {
    return leafNew(tree, obj, tree->spatialIndex.bbfunc(obj));
}

SpatialIndex *BBTreeInit(BBTree *tree, SpatialIndexBBFunc bbfunc, SpatialIndex *staticIndex) {
    spatialIndexInit((SpatialIndex *) tree, Klass(), bbfunc, staticIndex);

    tree->velocityFunc = NULL;

    tree->leaves = hashSetNew(0, (HashSetEqlFunc) leafSetEql);
    tree->root = NULL;

    tree->pooledNodes = NULL;
    tree->allocatedBuffers = arrayNew(0);

    tree->stamp = 0;

    return (SpatialIndex *) tree;
}

void BBTreeSetVelocityFunc(SpatialIndex *index, BBTreeVelocityFunc func) {
    if (index->klass != Klass()) {
        assertWarn(false, VOID_ERR, "Ignoring BBTreeSetVelocityFunc() call to non-tree spatial index.");
        return;
    }

    ((BBTree *) index)->velocityFunc = func;
}

SpatialIndex *BBTreeNew(SpatialIndexBBFunc bbfunc, SpatialIndex *staticIndex) {
    return BBTreeInit(BBTreeAlloc(), bbfunc, staticIndex);
}

static void BBTreeDestroy(BBTree *tree) {
    hashSetFree(tree->leaves);

    if (tree->allocatedBuffers) arrayFreeEach(tree->allocatedBuffers, free);
    arrayFree(tree->allocatedBuffers);
}

// Misc - basic tree operations needed for klass

static int BBTreeCount(BBTree *tree) {
    return hashSetCount(tree->leaves);
}

typedef struct EachContext {
    SpatialIndexIteratorFunc func;
    void *data;
} EachContext;

static void eachHelper(Node *node, EachContext *context) {
    context->func(node->obj, context->data);
}

static void BBTreeEach(BBTree *tree, SpatialIndexIteratorFunc func, void *data) {
    EachContext context = {func, data};
    hashSetEach(tree->leaves, (HashSetIteratorFunc) eachHelper, &context);
}

// Insert/remove

static void BBTreeInsert(BBTree *tree, void *obj, HashValue hashId) {
    Node *leaf = (Node *) hashSetInsert(tree->leaves, hashId, obj, (HashSetTransFunc) leafSetTrans, tree);

    Node *root = tree->root;
    tree->root = subtreeInsert(root, leaf, tree);

    leaf->STAMP = getMasterTree(tree)->stamp;
    leafAddPairs(leaf, tree);
    incrementStamp(tree);
}

static void BBTreeRemove(BBTree *tree, void *obj, HashValue hashId) {
    Node *leaf = (Node *) hashSetRemove(tree->leaves, hashId, obj);

    tree->root = subtreeRemove(tree->root, leaf, tree);
    pairsClear(leaf, tree);
    nodeRecycle(tree, leaf);
}

static bool BBTreeContains(BBTree *tree, void *obj, HashValue hashId) {
    return (hashSetFind(tree->leaves, hashId, obj) != NULL);
}

static void leafUpdateWrap(Node *leaf, BBTree *tree) {
    leafUpdate(leaf, tree);
}

static void BBTreeReindexQuery(BBTree *tree, SpatialIndexQueryFunc func, void *data) {
    if (!tree->root) return;

    // leafUpdate() may modify tree->root. Don't cache it.
    hashSetEach(tree->leaves, (HashSetIteratorFunc) leafUpdateWrap, tree);

    SpatialIndex *staticIndex = tree->spatialIndex.staticIndex;
    Node *staticRoot = (staticIndex && staticIndex->klass == Klass() ?
    ((BBTree *) staticIndex)->root : NULL);

    MarkContext context = {tree, staticRoot, func, data};
    markSubtree(tree->root, &context);
    if (staticIndex && !staticRoot) spatialIndexCollideStatic((SpatialIndex *) tree, staticIndex, func, data);

    incrementStamp(tree);
}

static void BBTreeReindex(BBTree *tree) {
    BBTreeReindexQuery(tree, voidQueryFunc, NULL);
}

static void BBTreeReindexObject(BBTree *tree, void *obj, HashValue hashId) {
    Node *leaf = (Node *) hashSetFind(tree->leaves, hashId, obj);
    if (leaf) {
        if (leafUpdate(leaf, tree)) leafAddPairs(leaf, tree);
        incrementStamp(tree);
    }
}

// KLASS

static SpatialIndexClass klass = {
    (SpatialIndexDestroyImpl) BBTreeDestroy,

    (SpatialIndexCountImpl) BBTreeCount,
    (SpatialIndexEachImpl) BBTreeEach, //

    (SpatialIndexContainsImpl) BBTreeContains,
    (SpatialIndexInsertImpl) BBTreeInsert,
    (SpatialIndexRemoveImpl) BBTreeRemove,

    (SpatialIndexReindexImpl) BBTreeReindex,
    (SpatialIndexReindexObjectImpl) BBTreeReindexObject,
	(SpatialIndexReindexQueryImpl) BBTreeReindexQuery,

    /* NOTE: Necessary?
	(cpSpatialIndexQueryImpl)cpBBTreeQuery,
	(cpSpatialIndexSegmentQueryImpl)cpBBTreeSegmentQuery,
    */
};

static inline SpatialIndexClass *Klass() {return &klass;}

// NOTE: Optimization functions have not been implemented.
