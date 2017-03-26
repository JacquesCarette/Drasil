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

 // COLLISION_C - COLLISION MODULE //

#include <stdio.h>
#include <string.h>

#include "../include/Chipmunk.h"

// Originally from cpRobust.h/.c:
// Check that the signed area of the triangle a, b, c is positive.
static inline bool checkSignedArea(const Vector a, const Vector b, const Vector c) {
    const Vector v0 = vectSub(b, a);
    const Vector v1 = vectAdd(vectSub(c, a), vectSub(c, b));
    return (v0.x * v1.y) > (v1.x * v0.y);
}

// Functions and definitions from cpCollision.c:

#define MAX_GJK_ITERATIONS 30
#define MAX_EPA_ITERATIONS 30
#define WARN_GJK_ITERATIONS 20
#define WARN_EPA_ITERATIONS 20

static inline void collisionInfoPushContact(struct CollisionInfo *info, Vector p1, Vector p2, HashValue hash) {
    assertSoft(info->count <= MAX_CONTACTS_PER_ARBITER, VOID_ERR, "Internal Error: Tried to push too many contacts.");

    struct Contact *con = &info->arr[info->count];
    con->r1 = p1;
    con->r2 = p2;
    con->hash = hash;

    ++info->count;
}

// Support Points and Edges:
// Support points: the maximal points on a shape's perimeter along a certain axis.
// The GJK and EPA algorithms use support points to iteratively sample the surface of the two shapes' Minkowski difference.

static inline int polySupportPointIndex(const int count, const struct SplittingPlane *planes, const Vector n) {
    double max = -INFINITY;
    int index = 0;

    for (int i = 0; i < count; ++i) {
        Vector v = planes[i].v0;
        double d = vectDot(v, n);

        if (d > max) {
            max = d;
            index = i;
        }
    }

    return index;
}

struct SupportPoint {
    Vector p;
    // Save an index of the point so it can be cheaply looked up as a starting point for the next frame.
    CollisionID index;
};

static inline struct SupportPoint supportPointNew(Vector p, CollisionID index) {
    struct SupportPoint point = {p, index};
    return point;
}

typedef struct SupportPoint (*SupportPointFunc)(const Shape *shape, const Vector n);

static inline struct SupportPoint circleSupportPoint(const CircleShape *circle, const Vector n) {
    return supportPointNew(circle->tcenter, 0);
}

static inline struct SupportPoint segmentSupportPoint(const SegmentShape *seg, const Vector n) {
    if (vectDot(seg->ta, n) > vectDot(seg->tb, n)) {
        return supportPointNew(seg->ta, 0);
    } else {
        return supportPointNew(seg->tb, 1);
    }
}

static inline struct SupportPoint polySupportPoint(const PolyShape *poly, const Vector n) {
    const struct SplittingPlane *planes = poly->planes;
    int i = polySupportPointIndex(poly->count, planes, n);
    return supportPointNew(planes[i].v0, i);
}

// A point on the surface of two shapes' Minkowski difference.
struct MinkowskiPoint {
    // Cache the two original support points.
    Vector a, b;
    // b - a
    Vector ab;
    // Concatenate the two support point indices.
    CollisionID id;
};

static inline struct MinkowskiPoint minkowskiPointNew(const struct SupportPoint a, const struct SupportPoint b) {
    struct MinkowskiPoint point = {a.p, b.p, vectSub(b.p, a.p), (a.index & 0xFF)<<8 | (b.index & 0xFF)};
    return point;
}

struct SupportContext {
    const Shape *shape1, *shape2;
    SupportPointFunc func1, func2;
};

// Calculate the maximal point on the Minkowski difference of two shapes along a particular axis.

static inline struct MinkowskiPoint support(const struct SupportContext *ctx, const Vector n) {
    struct SupportPoint a = ctx->func1(ctx->shape1, vectNeg(n));
    struct SupportPoint b = ctx->func2(ctx->shape2, n);
    return minkowskiPointNew(a, b);
}

struct EdgePoint {
    Vector p;
    // Keep a hash value for Chipmunk's collision hashing mechanism.
    HashValue hash;
};

// Support edges are the edges of a polygon or segment shape that are in contact.
struct Edge {
    struct EdgePoint a, b;
    double radius;
    Vector normal;
};

static struct Edge supportEdgeForPoly(const PolyShape *poly, const Vector n) {
    int count = poly->count;
    int i1 = polySupportPointIndex(poly->count, poly->planes, n);

    // Creator's TODO: get rid of mod eventually, very expensive on ARM.
    int i0 = (i1 - 1 + count) % count;
    int i2 = (i1 + 1) % count;

    const struct SplittingPlane *planes = poly->planes;
    HashValue hashId = poly->shape.hashId;

    if (vectDot(n, planes[i1].n) > vectDot(n, planes[i2].n)) {
        struct Edge edge = {
            {planes[i0].v0, HASH_PAIR(hashId, i0)},
            {planes[i1].v0, HASH_PAIR(hashId, i1)},
            poly->radius, planes[i1].n
        };
        return edge;
    } else {
        struct Edge edge = {
            {planes[i1].v0, HASH_PAIR(hashId, i1)},
            {planes[i2].v0, HASH_PAIR(hashId, i2)},
            poly->radius, planes[i1].n
        };
        return edge;
    }
}

static struct Edge supportEdgeForSegment(const SegmentShape *seg, const Vector n) {
    HashValue hashId = seg->shape.hashId;

    if (vectDot(seg->tn, n) > 0.0) {
        struct Edge edge = {
            {seg->ta, HASH_PAIR(hashId, 0)},
            {seg->tb, HASH_PAIR(hashId, 1)},
            seg->radius, seg->tn
        };
        return edge;
    } else {
        struct Edge edge = {
            {seg->tb, HASH_PAIR(hashId, 1)},
            {seg->ta, HASH_PAIR(hashId, 0)},
            seg->radius, vectNeg(seg->tn)
        };
        return edge;
    }
}

// Find the closest p(t) to (0, 0) where p(t) = a*(1-t)/2 + b*(1+t)/2.
// The range for t is [-1, 1] to avoid floating point issues if the parameters are swapped.
static inline double closestT(const Vector a, const Vector b)
{
	Vector delta = vectSub(b, a);
	return -fclamp(vectDot(delta, vectAdd(a, b)) / vectLengthSq(delta), -1.0f, 1.0f);
}

// Basically the same as vectLerp(), except t = [-1, 1].
static inline Vector lerpT(const Vector a, const Vector b, const double t) {
    double ht = 0.5f * t;
    return vectAdd(vectMult(a, 0.5f - ht), vectMult(b, 0.5f + ht));
}

// Closest points on the surface of two shapes.
struct ClosestPoints {
    // Surface points in absolute coordinates.
    Vector a, b;
    // Minimum separating axis of the two shapes.
    Vector n;
    // Signed distance between the points.
    double d;
    // Concatenation of the ids of the Minkowski points.
    CollisionID id;
};

// Error value for unit testing.
#ifdef UNIT_TEST
    static const struct errorPoints = {VECT_ERR, VECT_ERR, DBL_MIN, UINT32_MAX};
    #define POINTS_ERR errorPoints
#endif

// Calculate the closest points on two shapes given the closest edge on their Minkowski difference to (0, 0).
static inline struct ClosestPoints closestPointsNew(const struct MinkowskiPoint v0, const struct MinkowskiPoint v1)
{
	// Find the closest p(t) on the Minkowski difference to (0, 0).
	double t = closestT(v0.ab, v1.ab);
	Vector p = lerpT(v0.ab, v1.ab, t);

	// Interpolate the original support points using the same 't' value as above.
	// This gives you the closest surface points in absolute coordinates. NEAT!
	Vector pa = lerpT(v0.a, v1.a, t);
	Vector pb = lerpT(v0.b, v1.b, t);
	CollisionID id = (v0.id & 0xFFFF)<<16 | (v1.id & 0xFFFF);

	// First try calculating the MSA from the Minkowski difference edge.
	// This gives us a nice, accurate MSA when the surfaces are close together.
	Vector delta = vectSub(v1.ab, v0.ab);
    Vector normal = vectNormalize(vectRPerp(delta));
    double d = vectDot(normal, p);

	if(d <= 0.0f || (-1.0f < t && t < 1.0f)){
		// If the shapes are overlapping, or we have a regular vertex/edge collision, we are done.
        // NOTE: For this simplified implementation, we are actually only considering this kind of collision. However, I will keep the else case for now so I don't break the code.
		struct ClosestPoints points = {pa, pb, normal, d, id};
		return points;
	} else {
		// Vertex/vertex collisions need special treatment since the MSA won't be shared with an axis of the minkowski difference.
		double d2 = vectLength(p);
		Vector n2 = vectMult(p, 1.0f / (d2 + DBL_MIN));

		struct ClosestPoints points = {pa, pb, n2, d2, id};
		return points;
	}
}

// EPA functions.

static inline double closestDist(const Vector v0, const Vector v1) {
    return vectLengthSq(lerpT(v0, v1, closestT(v0, v1)));
}

// Recursive implementation of the EPA loop.
// Each recursion adds a point to the convex hull until it is known that we have the closest point on the surface.
static struct ClosestPoints EPARecurse(const struct SupportContext *ctx, const int count, const struct MinkowskiPoint *hull, const int iteration) {
    int mini = 0;
    double mindist = INFINITY;

    // Creator's TODO: precalculate this when building the hull and save a step.
    // Find the closest segment hull[i] and hull[i + 1] to (0, 0).
    for (int j = 0, i = count - 1; j < count; i = j, j++) {
        double dist = closestDist(hull[i].ab, hull[j].ab);
        if (dist < mindist) {
            mindist = dist;
            mini = i;
        }
    }

    struct MinkowskiPoint v0 = hull[mini];
    struct MinkowskiPoint v1 = hull[(mini + 1) % count];
    assertSoft(!vectEqual(v0.ab, v1.ab), POINTS_ERR, "Internal Error: EPA vertices are the same (%d and %d).", mini, (mini + 1) % count);

    // Check if there's a point on the Minkowski difference beyond this edge.
    struct MinkowskiPoint p = support(ctx, vectPerp(vectSub(v1.ab, v0.ab)));

    // The usual exit condition is a duplicated vertex.
    // Much faster to check the ids than to check the signed area.
    bool duplicate = (p.id == v0.id || p.id == v1.id);

    if (!duplicate && checkSignedArea(v0.ab, v1.ab, p.ab) && iteration < MAX_EPA_ITERATIONS) {
        // Rebuild the convex hull by inserting p.
        struct MinkowskiPoint *hull2 = (struct MinkowskiPoint *) alloca((count + 1) * sizeof(struct MinkowskiPoint));
        int count2 = 1;
        hull2[0] = p;

        for (int i = 0; i < count; ++i) {
            int index = (mini + 1 + i) % count;

            Vector h0 = hull2[count2 - 1].ab;
            Vector h1 = hull[index].ab;
            Vector h2 = (i + 1 < count ? hull[(index + 1) % count] : p).ab;

            if (checkSignedArea(h0, h2, h1)) {
                hull2[count2] = hull[index];
                ++count2;
            }
        }

        return EPARecurse(ctx, count2, hull2, iteration + 1);
    } else {
        // Could not find a new point to insert, so we have found the closest edge of the minkowski difference.
        assertWarn(iteration < WARN_EPA_ITERATIONS, POINTS_ERR, "High EPA iterations: %d", iteration);
        return closestPointsNew(v0, v1);
    }
}

// Find the closest points on the surface of two overlapping shapes using the EPA algorithm.
// EPA is called from GJK when two shapes overlap.
// This is a moderately expensive step! Avoid it by adding radii to your shapes so their inner polygons won't overlap.

static struct ClosestPoints EPA(const struct SupportContext *ctx, const struct MinkowskiPoint v0, const struct MinkowskiPoint v1, const struct MinkowskiPoint v2) {
    // Creator's NOTE: TODO: allocate a NxM array here and do an in place convex hull reduction in EPARecurse
    struct MinkowskiPoint hull[3] = {v0, v1, v2};
    return EPARecurse(ctx, 3, hull, 1);
}

// GJK functions.

static inline bool checkArea(Vector v1, Vector v2) {
    return (v1.x * v2.y) > (v1.y * v2.x);
}

// Recursive implementation of the GJK loop.
static inline struct ClosestPoints GJKRecurse(const struct SupportContext *ctx, const struct MinkowskiPoint v0, const struct MinkowskiPoint v1, const int iteration) {
    if (iteration > MAX_GJK_ITERATIONS) {
        assertWarn(iteration < WARN_GJK_ITERATIONS, POINTS_ERR, "High GJK iterations: %d", iteration);
        return closestPointsNew(v0, v1);
    }

    Vector delta = vectSub(v1.ab, v0.ab);

    if (checkArea(delta, vectAdd(v0.ab, v1.ab))) {
        // Origin is behind axis. Flip and try again.
        return GJKRecurse(ctx, v1, v0, iteration);
    } else {
        double t = closestT(v0.ab, v1.ab);
        Vector normal = (-1.0f < t && t < 1.0f ? vectPerp(delta) : vectNeg(lerpT(v0.ab, v1.ab, t)));
        struct MinkowskiPoint p = support(ctx, normal);

        if (
            checkArea(vectSub(v1.ab, p.ab), vectAdd(v1.ab, p.ab)) &&
            checkArea(vectAdd(v0.ab, p.ab), vectSub(v0.ab, p.ab))
        ) {
            // The triangle v0, p, v1 contains the origin. Use EPA to find MSA.
            assertWarn(iteration < WARN_GJK_ITERATIONS, POINTS_ERR, "High GJK->EPA iterations: %d", iteration);
            return EPA(ctx, v0, p, v1);
        } else {
            if (vectDot(p.ab, normal) <= fmax(vectDot(v0.ab, normal), vectDot(v1.ab, normal))) {
                // The edge v0, v1 that we arleady have is the closest to (0, 0) since p was not closer.
                assertWarn(iteration < WARN_GJK_ITERATIONS, POINTS_ERR, "High GJK iterations: %d", iteration);
                return closestPointsNew(v0, v1);
            } else {
                // p was closer to the origin than our existing edge.
                // Need to figure out which existing point to drop.
                if (closestDist(v0.ab, p.ab) < closestDist(p.ab, v1.ab)) {
                    return GJKRecurse(ctx, v0, p, iteration + 1);
                } else {
                    return GJKRecurse(ctx, p, v1, iteration + 1);
                }
            }
        }
    }
}

// Get a SupportPoint from a cached shape and index.
static struct SupportPoint shapePoint(const Shape *shape, const int i) {
    switch (shape->klass->type) {
        case CIRCLE_SHAPE: {
            return supportPointNew(((CircleShape *) shape)->tcenter, 0);
        } case SEGMENT_SHAPE: {
            SegmentShape *seg = (SegmentShape *) shape;
            return supportPointNew(i == 0 ? seg->ta : seg->tb, i);
        } case POLY_SHAPE : {
            PolyShape *poly = (PolyShape *) shape;
            // Poly shapes may change vertex count.
            int index = (i < poly->count ? i : 0);
            return supportPointNew(poly->planes[index].v0, index);
        } default: {
            return supportPointNew(zeroVect, 0);
        }
    }
}

// Find the closest points between two shapes using the GJK algorithm.
static struct ClosestPoints GJK(const struct SupportContext *ctx, CollisionID *id) {
    struct MinkowskiPoint v0, v1;

    if (*id) {
        // Use the Minkowski points from the last frame as a starting point using the cached indices.
        v0 = minkowskiPointNew(shapePoint(ctx->shape1, (*id>>24)&0xFF), shapePoint(ctx->shape2, (*id>>16)&0xFF));
        v1 = minkowskiPointNew(shapePoint(ctx->shape1, (*id>> 8)&0xFF), shapePoint(ctx->shape2, (*id    )&0xFF));
    } else {
        // No cached indices, use the shapes' bounding box centers as a guess for a starting axis.
        Vector axis = vectPerp(vectSub(BBCenter(ctx->shape1->bb), BBCenter(ctx->shape2->bb)));
        v0 = support(ctx, axis);
        v1 = support(ctx, vectNeg(axis));
    }

    struct ClosestPoints points = GJKRecurse(ctx, v0, v1, 1);
    *id = points.id;

    return points;
}

// Contact Clipping.

// Given two support edges, find contact point pairs on their surfaces.
static inline void contactPoints(const struct Edge e1, const struct Edge e2, const struct ClosestPoints points, struct CollisionInfo *info) {
    double mindist = e1.radius + e2.radius;

    if (points.d <= mindist) {
        Vector normal = info->normal = points.n;

        // Distances along the axis parallel to n.
        double d_e1_a = vectCross(e1.a.p, normal);
        double d_e1_b = vectCross(e1.b.p, normal);
        double d_e2_a = vectCross(e2.a.p, normal);
        double d_e2_b = vectCross(e2.b.p, normal);

        double e1_denom = 1.0f / (d_e1_b - d_e1_a);
        double e2_denom = 1.0f / (d_e2_b - d_e2_a);

        // Project the endpoints of the two edges onto the opposing edge, clamping them as necessary.
        // Compare the projected points to the collision normal to see if the shapes overlap there.

        {
            Vector p1 = vectAdd(vectMult(normal, e1.radius), vectLerp(e1.a.p, e1.b.p, fclamp01((d_e2_b - d_e1_a) * e1_denom)));
            Vector p2 = vectAdd(vectMult(normal, -e2.radius), vectLerp(e2.a.p, e2.b.p, fclamp01((d_e1_a - d_e2_a) * e2_denom)));
            double dist = vectDot(vectSub(p2, p1), normal);

            if (dist <= 0.0f) {
                HashValue hash_1a2b = HASH_PAIR(e1.a.hash, e2.b.hash);
                collisionInfoPushContact(info, p1, p2, hash_1a2b);
            }
        }{
            Vector p1 = vectAdd(vectMult(normal, e1.radius), vectLerp(e1.a.p, e1.b.p, fclamp01((d_e2_a - d_e1_a) * e1_denom)));
            Vector p2 = vectAdd(vectMult(normal, -e2.radius), vectLerp(e2.a.p, e2.b.p, fclamp01((d_e1_b - d_e2_a) * e2_denom)));
            double dist = vectDot(vectSub(p2, p1), normal);

            if (dist <= 0.0f) {
                HashValue hash_1b2a = HASH_PAIR(e1.b.hash, e2.a.hash);
                collisionInfoPushContact(info, p1, p2, hash_1b2a);
            }
        }
    }
}

// Collision functions.

typedef void (*CollisionFunc)(const Shape *a, const Shape *b, struct CollisionInfo *info);

static void CircleToCircle(const CircleShape *c1, const CircleShape *c2, struct CollisionInfo *info) {
    double mindist = c1->radius + c2->radius;
    Vector delta = vectSub(c2->tcenter, c1->tcenter);
    double distsq = vectLengthSq(delta);

    if (distsq < mindist * mindist) {
        double dist = sqrt(distsq);
        Vector normal = info->normal = (dist ? vectMult(delta, 1.0f / dist) : vect(1.0f, 0.0f));
        collisionInfoPushContact(info, vectAdd(c1->tcenter, vectMult(normal, c1->radius)), vectAdd(c2->tcenter, vectMult(normal, -c2->radius)), 0);
    }
}

static void CircleToSegment(const CircleShape *circle, const SegmentShape *segment, struct CollisionInfo *info) {
    Vector seg_a = segment->ta;
    Vector seg_b = segment->tb;
    Vector center = circle->tcenter;

    // Find the closest point on the segment to the circle.
    Vector seg_delta = vectSub(seg_b, seg_a);
    double closest_t = fclamp01(vectDot(seg_delta, vectSub(center, seg_a)) / vectLengthSq(seg_delta));
    Vector closest = vectAdd(seg_a, vectMult(seg_delta, closest_t));

    // Compare the radii of the two shapes to see if they are colliding.
    double mindist = circle->radius + segment->radius;
    Vector delta = vectSub(closest, center);
    double distsq = vectLengthSq(delta);

    if (distsq < mindist * mindist) {
        double dist = sqrt(distsq);
        // Handle coincident shapes as gracefully as possible.
        Vector normal = info->normal = (dist ? vectMult(delta, 1.0f / dist) : segment->tn);

        // Reject endcap collisions if tangents are provided.
        Vector rot = bodyGetRotation(segment->shape.body);
        if (
            (closest_t != 0.0f || vectDot(normal, vectRotate(segment->aTangent, rot)) >= 0.0) &&
            (closest_t != 0.0f || vectDot(normal, vectRotate(segment->bTangent, rot)) >= 0.0)
        ) {
            collisionInfoPushContact(info, vectAdd(center, vectMult(normal, circle->radius)), vectAdd(closest, vectMult(normal, -segment->radius)), 0);
        }
    }
}

static void SegmentToSegment(const SegmentShape *seg1, const SegmentShape *seg2, struct CollisionInfo *info)
{
	struct SupportContext context = {
        (Shape *) seg1,
        (Shape *) seg2,
        (SupportPointFunc) segmentSupportPoint,
        (SupportPointFunc) segmentSupportPoint
    };
	struct ClosestPoints points = GJK(&context, &info->id);

	Vector normal = points.n;
	Vector rot1 = bodyGetRotation(seg1->shape.body);
	Vector rot2 = bodyGetRotation(seg2->shape.body);

	// If the closest points are nearer than the sum of the radii:
	if(
		points.d <= (seg1->radius + seg2->radius) &&
		(
			// Reject endcap collisions if tangents are provided.
			(!vectEqual(points.a, seg1->ta) || vectDot(normal, vectRotate(seg1->aTangent, rot1)) <= 0.0) &&
			(!vectEqual(points.a, seg1->tb) || vectDot(normal, vectRotate(seg1->bTangent, rot1)) <= 0.0) &&
			(!vectEqual(points.b, seg2->ta) || vectDot(normal, vectRotate(seg2->aTangent, rot2)) >= 0.0) &&
			(!vectEqual(points.b, seg2->tb) || vectDot(normal, vectRotate(seg2->bTangent, rot2)) >= 0.0)
		)
	){
		contactPoints(supportEdgeForSegment(seg1, normal), supportEdgeForSegment(seg2, vectNeg(normal)), points, info);
	}
}

static void PolyToPoly(const PolyShape *poly1, const PolyShape *poly2, struct CollisionInfo *info) {
    struct SupportContext context = {
        (Shape *) poly1,
        (Shape *) poly2,
        (SupportPointFunc) polySupportPoint,
        (SupportPointFunc) polySupportPoint
    };
    struct ClosestPoints points = GJK(&context, &info->id);

    // If the closest points are nearer than the sum of the radii:
    if (points.d - poly1->radius - poly2->radius <= 0.0) {
        contactPoints(supportEdgeForPoly(poly1, points.n), supportEdgeForPoly(poly2, vectNeg(points.n)), points, info);
    }
}

static void SegmentToPoly(const SegmentShape *seg, const PolyShape *poly, struct CollisionInfo *info) {
    struct SupportContext context = {
        (Shape *) seg,
        (Shape *) poly,
        (SupportPointFunc) segmentSupportPoint,
        (SupportPointFunc) polySupportPoint
    };
    struct ClosestPoints points = GJK(&context, &info->id);

    Vector normal = points.n;
    Vector rot = bodyGetRotation(seg->shape.body);

    if (
        // If the closest points are nearer than the sum of the radii:
        points.d - seg->radius - poly->radius <= 0.0 &&
        (
            // Reject endcap collisions if tangents are provided.
            (!vectEqual(points.a, seg->ta) || vectDot(normal, vectRotate(seg->aTangent, rot)) <= 0.0) &&
            (!vectEqual(points.a, seg->tb) || vectDot(normal,
            vectRotate(seg->bTangent, rot)) <= 0.0)
        )
    ) {
        contactPoints(supportEdgeForSegment(seg, normal), supportEdgeForPoly(poly, vectNeg(normal)), points, info);
    }
}

static void CircleToPoly(const CircleShape *circle, const PolyShape *poly, struct CollisionInfo *info) {
    struct SupportContext context = {
        (Shape *) circle,
        (Shape *) poly,
        (SupportPointFunc) circleSupportPoint,
        (SupportPointFunc) polySupportPoint
    };
    struct ClosestPoints points = GJK(&context, &info->id);

    // If the closest points are nearer than the sum of the radii, then:
    if (points.d <= circle->radius + poly->radius) {
        Vector normal = info->normal = points.n;
        collisionInfoPushContact(info, vectAdd(points.a, vectMult(normal, circle->radius)), vectAdd(points.b, vectMult(normal, poly->radius)), 0);
    }
}

static void CollisionError(const Shape *circle, const Shape *poly, struct CollisionInfo *info) {
    assertHard(false, VOID_ERR, "Internal Error: Shape types are not sorted.");
}

static const CollisionFunc BuiltinCollisionFuncs[9] = {
    (CollisionFunc) CircleToCircle,
    CollisionError,
    CollisionError,
    (CollisionFunc) CircleToSegment,
    (CollisionFunc) SegmentToSegment,
    CollisionError,
    (CollisionFunc) CircleToPoly,
    (CollisionFunc) SegmentToPoly,
    (CollisionFunc) PolyToPoly,
};

static const CollisionFunc *CollisionFuncs = BuiltinCollisionFuncs;

struct CollisionInfo collide(const Shape *a, const Shape *b, CollisionID id, struct Contact *contacts)
{
	struct CollisionInfo info = {a, b, id, zeroVect, 0, contacts};

	// Make sure the shape types are in order.
	if (a->klass->type > b->klass->type){
		info.a = b;
		info.b = a;
	}

	CollisionFuncs[info.a->klass->type +
    info.b->klass->type*NUM_SHAPES](info.a, info.b, &info);

	return info;
}

ContactPointSet shapesCollide(const Shape *s1, const Shape *s2) {
    struct Contact contacts[MAX_CONTACTS_PER_ARBITER];
    struct CollisionInfo info = collide(s1, s2, 0, contacts);

    ContactPointSet set;
    set.count = info.count;

    // collideShapes() may have swapped the contact order.
    // Flip the normal.
    bool swapped = (s1 != info.a);
    set.normal = (swapped ? vectNeg(info.normal) : info.normal);

    for (int i = 0; i < info.count; i++) {
        //collideShapesInfo() returns contacts with absolute positions
        Vector p1 = contacts[i].r1;
        Vector p2 = contacts[i].r2;

        set.points[i].pointA = swapped ? p2 : p1;
        set.points[i].pointB = swapped ? p1 : p2;
        set.points[i].distance = vectDot(vectSub(p2, p1), set.normal);
    }

    return set;
}
