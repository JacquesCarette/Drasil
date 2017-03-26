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

 // BODY_H - RIGID BODY MODULE //

#ifndef BODY_H
#define BODY_H

// BodyType definitions - Kinematic is not included here, it seems
// unnecessary for the simulation.
typedef enum BodyType {
    // Dynamic bodies have a linear & angular position and velocity and
    // are affected by ALL world forces.
    DYNAMIC_BODY,
    // Static bodies are static - stays in one place (e.g. the ground),
    // should only be moved by reindexing, and don't produce collision
    // callbacks when colliding with other static bodies.
    STATIC_BODY
} BodyType;

// Position update integration function pointer type for rigid bodies
typedef void (*PositionFunc)(Body *body, double dt);
// Velocity update integration function pointer type for rigid bodies
typedef void (*VelocityFunc)(Body *body, Vector gravity, double dt);

// Memory management functions.
Body *bodyAlloc();
Body *bodyInit();

Body *newBody(const double mass, const double moment); // for dynamic bodies
Body *newStaticBody(); // for static bodies
// NOTE: Do NOT free a body before freeing all of its associated shapes and
// arbiters from the space!

void bodyDestroy(Body *body);

// Getters and setters for body type.
BodyType bodyGetType(Body *body);
void bodySetType(Body *body, const BodyType type);

// Get space associated with the body.
Space *bodyGetSpace(const Body *body);

// Getters and setters for mass.
double bodyGetMass(const Body *body);
void bodySetMass(Body *body, const double mass);

// Getters and setters for moment of inertia.
double bodyGetMoment(const Body *body);
void bodySetMoment(Body *body, const double moment);

// Getters and setters for position.
Vector bodyGetPosition(const Body *body);
void bodySetPosition(Body *body, const Vector position);

// Getters and setters for center of mass.
Vector bodyGetCenterOfMass(const Body *body);
void bodySetCenterOfMass(Body *body, const Vector com);

// Getters and setters for velocity.
Vector bodyGetVelocity(const Body *body);
void bodySetVelocity(Body *body, const Vector vel);

// Getters and setters for force.
Vector bodyGetForce(const Body *body);
void bodySetForce(Body *body, const Vector force);

// Getters and setters for angles.
double bodyGetAngle(const Body *body);
void bodySetAngle(Body *body, const double angle);

// Getters and setters for angular velocity.
double bodyGetAngularVelocity(const Body *body);
void bodySetAngularVelocity(Body *body, const double avel);

// Getters and setters for torque.
double bodyGetTorque(const Body *body);
void bodySetTorque(Body *body, const double torque);

// Returns the body's rotation vector.
Vector bodyGetRotation(const Body *body);

// Setters for position and velocity update functions.
void bodySetPositionFunc(Body *body, PositionFunc newPositionFunc);
void bodySetVelocityFunc(Body *body, VelocityFunc newVelocityFunc);

// Default position and velocity update functions.
void bodyUpdatePosition(Body *body, const double dt);
void bodyUpdateVelocity(Body *body, const Vector gravity, const double dt);

// Coordinate-specific and other coordinate conversion functions
// NOTE: Not sure if necessary, but will comment out for now.
/*
Vector bodyLocalToWorld(const Body *body, const Vector point);
Vector bodyWorldToLocal(const Body *body, const Vector point);

void bodyApplyForceAtWorldPoint(Body *body, const Vector force, const Vector point);
void bodyApplyForceAtLocalPoint(Body *body, const Vector force, const Vector point);

void bodyApplyImpulseAtWorldPoint(Body *body, const Vector impulse, const Vector point);
void bodyApplyImpulseAtLocalPoint(Body *body, const Vector impulse, const Vector point);

Vector bodyGetVelocityAtLocalPoint(const Body *body, Vector point);
Vector bodyGetVelocityAtWorldPoint(const Body *body, Vector point);
*/

// Calculates the body's kinetic energy.
double bodyKineticEnergy(const Body *body);

// Iterator function type definitions for shape and arbiters.
typedef void (*ShapeIteratorFunc)(Body *body, Shape *shape, void *data);
typedef void (*ArbiterIteratorFunc)(Body *body, Arbiter *arb, void *data);

// Custom iterators for each shape and arbiter of the body.
void bodyEachShape(Body *body, ShapeIteratorFunc func, void *data);
void bodyEachArbiter(Body *body, ArbiterIteratorFunc func, void *data);

#endif
