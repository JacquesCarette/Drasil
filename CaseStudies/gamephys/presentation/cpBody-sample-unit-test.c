#include <assert.h>
#include "cpBody.h"

int main(void)
{
    // set rigid body with 50.0 kg mass and 20.0 kgm^2 moment of inertia (MoI)
    cpBody *body = cpBodyNew(50.0f, 20.0f);
    // these should pass
    assert(body);
    assert(cpBodyGetMass(body) == 50.0);
    assert(cpBodyGetMoment(body) == 20.0);
    assert(cpBodyGetAngle(body) == 0.0);
    assert(cpBodyGetType(body) == CP_BODY_TYPE_DYNAMIC);
    // the following will fail as quantities must be nonnegative
    // cpBodySetMass(body, -50.0f);
    // cpBodySetMoment(body, -10.0f);
    // changes body type to static
    cpBodySetType(body, CP_BODY_TYPE_STATIC);
    // these should pass
    assert(cpBodyGetMass(body) == INFINITY);
    assert(cpBodyGetMoment(body) == INFINITY);
    // frees and destroys body
    cpBodyFree(body);
    // program should exit without memory leaks
}
