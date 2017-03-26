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

// BODY PRIVATE DATA //

#ifndef BODY_PRIVATE_H
#define BODY_PRIVATE_H

#include "../Chipmunk_types.h"

// Structure definition for rigid bodies.
struct Body {
    // Body type.
    BodyType type;

    // Integration functions.
    PositionFunc positionFunc;
    VelocityFunc velocityFunc;

    // Physical properties.
    double mass;
    double massInv;

    double moment;
    double momentInv;

    // Center of mass
    Vector com;

    // Linear motion properties.
    Vector pos;
    Vector vel;
    Vector force;

    // Angular motion properties.
    double angle;
    double avel;
    double torque;

    Vector velBias;
    double avelBias;

    Transform transform;

    Space *space;

    Shape *shapeList;
    Arbiter *arbiterList;

    // Excluded items: user data, constraints, sleeping.
};

// Adds and removes shapes to and from the body.
void bodyAddShape(Body *body, Shape *shape);
void bodyRemoveShape(Body *body, Shape *shape);

// Accumulates mass from shapes and sets this to the body's mass. Should only be
// called when shapes with mass info are modified, added or removed.
void bodyAccumulateMassFromShapes(Body *body);

#endif
