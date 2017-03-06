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

 // BODY_C - RIGID BODY MODULE //

#include <float.h>
#include <stdarg.h>

#include "../include/Chipmunk.h"

Body *bodyAlloc() {
    return (Body *) calloc(1, sizeof(Body));
}

Body *bodyInit(Body *body, double mass, double moment) {
    body->space = NULL;
    body->shapeList = NULL;
    body->arbiterList = NULL;

    body->positionFunc = bodyUpdatePosition;
    body->velocityFunc = bodyUpdateVelocity;

    body->pos = zeroVect;
    body->vel = zeroVect;
    body->force = zeroVect;

    body->avel = 0.0f;
    body->torque = 0.0f;

    body->velBias = zeroVect;
    body->avelBias = 0.0f;

    bodySetMass(body, mass);
    bodySetMoment(body, moment);
    bodySetAngle(body, 0.0f);

    return body;
}

Body *newBody(const double mass, const double moment) {
    Body *newBody = bodyAlloc();
    newBody->type = DYNAMIC_BODY;
    bodyInit(newBody, mass, moment);

    return newBody;
}

Body *newStaticBody() {
    Body *newBod = newBody(0.0f, 0.0f);
    bodySetType(newBod, STATIC_BODY);

    return newBod;
}

void bodyDestroy(Body *body) {
    if (body) {
        free(body);
    }
}

// Body sanity checks:
static void vectAssertNaN(Vector v, char *msg) {
    assertHard(v.x == v.x && v.y == v.y, VOID_ERR, msg);
}

static void vectAssertInfinite(Vector v, char *msg) {
    assertHard(fabs(v.x) != INFINITY && fabs(v.y) != INFINITY, VOID_ERR, msg);
}

static void vectAssertSane(Vector v, char *msg) {
    vectAssertNaN(v, msg);
    vectAssertInfinite(v, msg);
}

static void assertSaneBody(const Body *body) {
    assertHard(body->mass == body->mass && body->massInv == body->massInv, VOID_ERR, "Body's mass is NaN.");
    assertHard(body->moment == body->moment && body->momentInv == body->momentInv, VOID_ERR, "Body's moment is NaN.");
    assertHard(body->mass >= 0.0f, VOID_ERR, "Body's mass is negative.");
    assertHard(body->moment >= 0.0f, VOID_ERR, "Body's moment is negative.");

    vectAssertSane(body->pos, "Body's position is invalid.");
    vectAssertSane(body->vel, "Body's velocity is invalid.");
    vectAssertSane(body->force, "Body's force is invalid.");

    assertHard(body->angle == body->angle && fabs(body->angle) != INFINITY, VOID_ERR, "Body's angle is invalid.");
    assertHard(body->avel == body->avel && fabs(body->avel) != INFINITY, VOID_ERR, "Body's angular velocity is invalid.");
    assertHard(body->torque == body->torque && fabs(body->torque) != INFINITY, VOID_ERR, "Body's torque is invalid.");
}

BodyType bodyGetType(Body *body) {
    return body->type;
}

void bodySetType(Body *body, const BodyType newType) {
    BodyType oldType = bodyGetType(body);

    // If the new type is the same as the old type, do nothing.
    if (oldType == newType) return;
    body->type = newType;

    // Change the body's properties.
    if (newType == DYNAMIC_BODY) {
        body->mass = 0.0f;
        body->moment = 0.0f;
        body->massInv = INFINITY;
        body->momentInv = INFINITY;

        bodyAccumulateMassFromShapes(body);
    } else {
        body->mass = INFINITY;
        body->moment = INFINITY;
        body->massInv = 0.0f;
        body->momentInv = 0.0f;

        body->vel = zeroVect;
        body->avel = 0.0f;
    }

    // If the body has been added to a space, update the space.
    Space *space = bodyGetSpace(body);
    if (space) {

        // Move the bodies to the correct array.
        Array *fromArr = spaceArrayForBodyType(space, oldType);
        Array *toArr = spaceArrayForBodyType(space, newType);
        if (fromArr != toArr) {
            arrayDeleteObj(fromArr, body);
            arrayPush(toArr, body);
        }

        // Move the body's shapes to the correct spatial index.
        SpatialIndex *fromIndex = (oldType == STATIC_BODY ? space->staticShapes
            : space->dynamicShapes);
        SpatialIndex *toIndex = (newType == STATIC_BODY ? space->staticShapes
            : space->dynamicShapes);
        if (fromIndex != toIndex) {
            for (Shape *shape = body->shapeList; shape; shape = shape->next) {
                spatialIndexRemove(fromIndex, shape, shape->hashId);
                spatialIndexInsert(toIndex, shape, shape->hashId);
            }
        }
    }
}

// See header file for notes:
void bodyAccumulateMassFromShapes(Body *body) {
    if (!body || bodyGetType(body) != DYNAMIC_BODY) return;

    // Reset body's mass.
    body->mass = 0.0f;
    body->moment = 0.0f;
    body->com = zeroVect;

    // Cache position for realigning at the end.
    Vector pos = bodyGetPosition(body);

    // Accumulate mass from shapes.
    for (Shape *shape = body->shapeList; shape; shape = shape->next) {
        ShapeMassInfo *info = &shape->massInfo;
        double mass = info->mass;

        if (mass > 0.0f) {
            double massSum = body->mass + mass;
            body->moment += mass * info->moment + vectDistSq(body->com, info->com) * (mass * body->mass) / massSum;
            body->com = vectLerp(body->com, info->com, mass/massSum);
            body->mass = massSum;
        }
    }

    // Recalculate inverses.
    body->massInv = 1.0f/body->mass;
    body->momentInv = 1.0f/body->moment;

    // Realign body.
    bodySetPosition(body, pos);
    assertSaneBody(body);
}

Space *bodyGetSpace(const Body *body) {
    return body->space;
}

double bodyGetMass(const Body *body) {
    return body->mass;
}

void bodySetMass(Body *body, const double mass) {
    assertHard(bodyGetType(body) == DYNAMIC_BODY, VOID_ERR,
        "You cannot set the mass of static bodies.");
    assertHard(0.0f <= mass && mass < INFINITY, VOID_ERR,
        "Mass must be positive and finite.");

    body->mass = mass;
    body->massInv = 1.0f / mass;
    assertSaneBody(body);
}

double bodyGetMoment(const Body *body) {
    return body->moment;
}

void bodySetMoment(Body *body, const double moment) {
    assertHard(moment >= 0.0f, VOID_ERR,
        "Moment of inertia must be positive.");
    body->moment = moment;
    body->momentInv = 1.0f / moment;
    assertSaneBody(body);
}

Vector bodyGetRotation(const Body *body) {
    return vect(body->transform.a, body->transform.b);
}

void bodyAddShape(Body *body, Shape *shape) {
    Shape *next = body->shapeList;
    if (next) next->prev = shape;

    shape->next = next;
    body->shapeList = shape;

    if (shape->massInfo.mass > 0.0f) {
        bodyAccumulateMassFromShapes(body);
    }
}

void bodyRemoveShape(Body *body, Shape *shape) {
    Shape *prev = shape->prev;
    Shape *next = shape->next;

    if (prev) {
        prev->next = next;
    } else {
        body->shapeList = next;
    }

    if (next) {
        next->prev = prev;
    }

    shape->prev = NULL;
    shape->next = NULL;

    if (bodyGetType(body) == DYNAMIC_BODY && shape->massInfo.mass > 0.0f) {
        bodyAccumulateMassFromShapes(body);
    }
}

static void bodySetTransform(Body *body, const Vector pos, const double angle) {
    Vector rot = vectForAngle(angle);
    Vector c = body->com;

    body->transform = transformNewTranspose(
        rot.x, -rot.y, pos.x - (c.x * rot.x - c.y * rot.y),
        rot.y, rot.x, pos.y - (c.x * rot.y + c.y * rot.x)
    );
}


Vector bodyGetPosition(const Body *body) {
    return transformPoint(body->transform, zeroVect);
}

void bodySetPosition(Body *body, const Vector pos) {
    Vector npos = body->pos = vectAdd(transformVect(body->transform, body->com), pos);
    assertSaneBody(body);

    bodySetTransform(body, npos, body->angle);
}

Vector bodyGetCenterOfMass(const Body *body) {
    return body->com;
}

void bodySetCenterOfMass(Body *body, const Vector com) {
    body->com = com;
    assertSaneBody(body);
}

Vector bodyGetVelocity(const Body *body) {
    return body->vel;
}

void bodySetVelocity(Body *body, const Vector vel) {
    body->vel = vel;
    assertSaneBody(body);
}

Vector bodyGetForce(const Body *body) {
    return body->force;
}

void bodySetForce(Body *body, const Vector force) {
    body->force = force;
    assertSaneBody(body);
}

double bodyGetAngle(const Body *body) {
    return body->angle;
}

void bodySetAngle(Body *body, const double angle) {
    body->angle = angle;
    assertSaneBody(body);

    bodySetTransform(body, body->pos, angle);
}

double bodyGetAngularVelocity(const Body *body) {
    return body->avel;
}

void bodySetAngularVelocity(Body *body, const double avel) {
    body->avel = avel;
    assertSaneBody(body);
}

double bodyGetTorque(const Body *body) {
    return body->torque;
}

void bodySetTorque(Body *body, const double torque) {
    body->torque = torque;
    assertSaneBody(body);
}

void bodySetPositionFunc(Body *body, PositionFunc newPositionFunc) {
    body->positionFunc = newPositionFunc;
}

void bodySetVelocityFunc(Body *body, VelocityFunc newVelocityFunc) {
    body->velocityFunc = newVelocityFunc;
}

void bodyUpdatePosition(Body *body, const double dt) {
    body->pos = vectAdd(body->pos, vectMult(vectAdd(body->vel, body->velBias), dt));
    double angle = body->angle + (body->avel + body->avelBias) * dt;
    bodySetAngle(body, angle);

    body->velBias = zeroVect;
    body->avelBias = 0.0f;

    assertSaneBody(body);
}

void bodyUpdateVelocity(Body *body, const Vector gravity, const double dt) {
    assertSoft(body->mass > 0.0f && body->moment > 0.0f, VOID_ERR,
        "Body's mass and moment must be positive to simulate. (Mass: %f \
         Moment: %f)", body->mass, body->moment);

    body->vel = vectAdd(body->vel, vectMult(vectAdd(gravity, vectMult(body->force, body->massInv)), dt));
    body->avel = body->avel + body->torque * body->momentInv * dt;

    // Reset forces
    body->force = zeroVect;
    body->torque = 0.0f;

    assertSaneBody(body);
}

double bodyKineticEnergy(const Body *body) {
    double velsq = vectDot(body->vel, body->vel);
    double avelsq = body->avel * body->avel;
    // NOTE: Why is it not divided by half in the original function?
    return 0.5f * ((velsq ? velsq * body->mass : 0.0f) + (avelsq ? avelsq * body->moment : 0.0f));
}

// Coordinate functions - see header file for notes.
/*
Vector bodyLocalToWorld(const Body *body, const Vector point) {
    return transformPoint(body->transform, point);
}

Vector bodyWorldToLocal(const Body *body, const Vector point) {
    return transformPoint(transformRigidInverse(body->transform), point);
}

void bodyApplyForceAtWorldPoint(Body *body, const Vector force, const Vector point) {
    body->force = vectAdd(body->force, force);

    Vector r = vectSub(point, transformPoint(body->transform, body->com));
    body->torque += vectCross(r, force);
}

void bodyApplyForceAtLocalPoint(Body *body, const Vector force, const Vector point) {
    bodyApplyForceAtWorldPoint(body, transformVect(body->transform, force), transformPoint(body->transform, point));
}

void bodyApplyImpulseAtWorldPoint(Body *body, const Vector impulse, const Vector point) {
    Vector r = vectSub(point, transformPoint(body->transform, body->com));
    body->vel = vectAdd(body->vel, vectMult(impulse, body->massInv));
    body->avel += body->momentInv * vectCross(r, impulse);
}

void bodyApplyImpulseAtLocalPoint(Body *body, const Vector impulse, const Vector point) {
    bodyApplyImpulseAtWorldPoint(body, transformVect(body->transform, impulse), transformPoint(body->transform, point));
}

Vector bodyGetVelocityAtLocalPoint(const Body *body, Vector point) {
    Vector r = transformVect(body->transform, vectSub(point, body->com));
    return vectAdd(body->vel, vectMult(vectRPerp(r), body->avel));
}

Vector bodyGetVelocityAtWorldPoint(const Body *body, Vector point) {
    Vector r = vectSub(point, transformPoint(body->transform, body->com));
    return vectAdd(body->vel, vectMult(vectRPerp(r), body->avel));
}
*/

// Custom iterators

void bodyEachShape(Body *body, ShapeIteratorFunc func, void *data) {
    Shape *shape = body->shapeList;

    while (shape) {
        Shape *next = shape->next;
        func(body, shape, data);
        shape = next;
    }
}

void bodyEachArbiter(Body *body, ArbiterIteratorFunc func, void *data) {
    Arbiter *arb = body->arbiterList;

    while (arb) {
        Arbiter *next = arbiterNext(arb, body);
        bool swapped = arb->swapped;
        arb->swapped = (body == arb->bodyB);
        func(body, arb, data);
        arb->swapped = swapped;

        arb = next;
    }
}
