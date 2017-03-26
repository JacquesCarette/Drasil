#include <stdio.h>
#include "Chipmunk.h"

// Replicating test program

static void println() {
    printf("\n");
}

int main(void) {
    // Initialize gravity

    Vector gravity = vect(0, -9.8);
    printf("Gravity initialized: (%0.2f, %0.2f)\n", gravity.x, gravity.y);
    println();

    // Create an empty space

    Space *space = spaceNew();
    printf("New space created...\n");
    spaceSetGravity(space, gravity);
    printf("Space gravity set: (%0.2f, %0.2f)\n", spaceGetGravity(space).x, spaceGetGravity(space).y);
    println();

    // Add a static line segment for the ground
    // Make it slightly tilted so that the ball rolls off
    // Attach this to space->staticBody so it shouldn't be movable

    Shape *ground = segmentShapeNew(spaceGetStaticBody(space), vect(-10000, 0), vect(10000, 0), 10);
    printf("New segment shape created for the ground...\n");
    shapeSetFriction(ground, 0.7);
    printf("Ground friction set to %0.2f.\n", shapeGetFriction(ground));
    spaceAddShape(space, ground);
    printf("Ground has been added to the space...\n");
    println();

    // Now let's make a ball that falls onto the line and rolls off.
    // First we need to make a cpBody to hold the physical properties of the object.
    // These include the mass, position, velocity, angle, etc. of the object.
    // Then we attach collision shapes to the cpBody to give it a size and shape.

    double innerRadius = 0;
    double outerRadius1 = 10;
    double outerRadius2 = 15;
    double outerRadius3 = 5;
    double mass = 10;

    // The moment of inertia is like mass for rotation
    // Use the momentFor*() functions to help you approximate it

    double moment1 = momentForCircle(mass, innerRadius, outerRadius1, zeroVect);
    double moment2 = momentForCircle(mass, innerRadius, outerRadius2, zeroVect);
    double moment3 = momentForCircle(mass, innerRadius, outerRadius3, zeroVect);

    // The spaceAdd*() functions return the thing that you are adding.
    // It's convenient to create and add an object in one line.

    Body *ballOneBody = spaceAddBody(space, newBody(mass, moment1));
    printf("Created first body with mass %0.2f kg and moment %0.2f kgm^2...\n", bodyGetMass(ballOneBody), bodyGetMoment(ballOneBody));
    bodySetPosition(ballOneBody, vect(-45, 45));
    printf("Set first body's position: (%0.2f, %0.2f)\n", bodyGetPosition(ballOneBody).x, bodyGetPosition(ballOneBody).y);
    bodySetVelocity(ballOneBody, vect(10, -9.8));
    printf("Set first body's velocity: (%0.2f, %0.2f)\n", bodyGetVelocity(ballOneBody).x, bodyGetVelocity(ballOneBody).y);
    println();

    Body *ballTwoBody = spaceAddBody(space, newBody(mass, moment2));
    printf("Created second body with mass %0.2f kg and moment %0.2f kgm^2...\n", bodyGetMass(ballTwoBody), bodyGetMoment(ballTwoBody));
    bodySetPosition(ballTwoBody, vect(45, 45));
    printf("Set second body's position: (%0.2f, %0.2f)\n", bodyGetPosition(ballTwoBody).x, bodyGetPosition(ballTwoBody).y);
    bodySetVelocity(ballTwoBody, vect(10, -9.8));
    printf("Set second body's velocity: (%0.2f, %0.2f)\n", bodyGetVelocity(ballTwoBody).x, bodyGetVelocity(ballTwoBody).y);
    println();

    Body *ballThreeBody = spaceAddBody(space, newBody(mass, moment3));
    printf("Created third body with mass %0.2f kg and moment %0.2f kgm^2...\n", bodyGetMass(ballThreeBody), bodyGetMoment(ballThreeBody));
    bodySetPosition(ballThreeBody, vect(0, 90));
    printf("Set third body's position: (%0.2f, %0.2f)\n", bodyGetPosition(ballThreeBody).x, bodyGetPosition(ballThreeBody).y);
    bodySetVelocity(ballThreeBody, vect(20, 0));
    printf("Set third body's velocity: (%0.2f, %0.2f)\n", bodyGetVelocity(ballThreeBody).x, bodyGetVelocity(ballThreeBody).y);
    println();

    // Now we create the collision shape for the ball.
    // You can create multiple collision shapes that point to the same body.
    // They will all be attached to the body and move around to follow it.

    Shape *ballOneShape = spaceAddShape(space, circleShapeNew(ballOneBody, outerRadius1, zeroVect));
    printf("Created first shape with radius %0.2f m and offset by (%0.2f, %0.2f)...\n", circleShapeGetRadius(ballOneShape), circleShapeGetOffset(ballOneShape).x, circleShapeGetOffset(ballOneShape).y);
    shapeSetFriction(ballOneShape, 0.2);
    printf("First shape's friction set to %0.2f...\n", shapeGetFriction(ballOneShape));
    println();

    Shape *ballTwoShape = spaceAddShape(space, circleShapeNew(ballTwoBody, outerRadius2, zeroVect));
    printf("Created second shape with radius %0.2f m and offset by (%0.2f, %0.2f)...\n", circleShapeGetRadius(ballTwoShape), circleShapeGetOffset(ballTwoShape).x, circleShapeGetOffset(ballTwoShape).y);
    shapeSetFriction(ballTwoShape, 0.5);
    printf("Second shape's friction set to %0.2f...\n", shapeGetFriction(ballTwoShape));
    println();

    Shape *ballThreeShape = spaceAddShape(space, circleShapeNew(ballThreeBody, outerRadius3, zeroVect));
    printf("Created third shape with radius %0.2f m and offset by (%0.2f, %0.2f)...\n", circleShapeGetRadius(ballThreeShape), circleShapeGetOffset(ballThreeShape).x, circleShapeGetOffset(ballThreeShape).y);
    shapeSetFriction(ballThreeShape, 0.5);
    printf("Second shape's friction set to %0.2f...\n", shapeGetFriction(ballThreeShape));
    println();

    // Now that it's all set up, we simulate all the objects in the space by
    // stepping forward through time in small incremends called steps.
    // It is *highly* recommended to use a fixed size time step.
    double timeStep = 1 / 60.0;
    double outputInterval = 5;
    double outputTime = 0.0;
    printf("Set timestep: %0.4f s\n", timeStep);
    printf("Set output interval: %0.4f s\n", outputInterval);
    printf("Set output time: %0.4f s\n", outputTime);
    println();

    printf("Running simulation...");
    println();

    for (double time = 0; time <= 100; time += timeStep) {
        Vector pos1 = bodyGetPosition(ballOneBody);
        Vector vel1 = bodyGetVelocity(ballOneBody);

        Vector pos2 = bodyGetPosition(ballTwoBody);
        Vector vel2 = bodyGetVelocity(ballTwoBody);

        Vector pos3 = bodyGetPosition(ballThreeBody);
        Vector vel3 = bodyGetVelocity(ballThreeBody);

        // Update info at specified time intervals and update the next time
        // interval.

        if (time >= outputTime) {
            printf(
              "Time is %5.2f. ballOneBody is at (%5.2f, %5.2f). Its velocity is (%5.2f, %5.2f)\n", time, pos1.x, pos1.y, vel1.x, vel1.y);
            printf(
              "Time is %5.2f. ballTwoBody is at (%5.2f, %5.2f). Its velocity is (%5.2f, %5.2f)\n", time, pos2.x, pos2.y, vel2.x, vel2.y);
            printf(
              "Time is %5.2f. ballThreeBody is at (%5.2f, %5.2f). Its velocity is (%5.2f, %5.2f)\n", time, pos3.x, pos3.y, vel3.x, vel3.y);
            println();
            outputTime += outputInterval;
        }

        spaceStep(space, timeStep);
    }

    // Clean up objects and exit

    shapeDestroy(ballOneShape);
    shapeDestroy(ballTwoShape);
    shapeDestroy(ballThreeShape);
    bodyDestroy(ballOneBody);
    bodyDestroy(ballTwoBody);
    bodyDestroy(ballThreeBody);
    shapeDestroy(ground);
    spaceFree(space);

    printf("Simulation finished.\n");
    printf("Goodbye!\n");
    return 0;
}
