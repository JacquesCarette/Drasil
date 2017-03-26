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

 // SHAPE PRIVATE DATA //

 #ifndef SHAPE_PRIVATE_H
 #define SHAPE_PRIVATE_H

 #include "../Chipmunk_types.h"

 #define MAGIC_EPSILON 1e-5

 typedef enum ShapeType{
     CIRCLE_SHAPE,
     SEGMENT_SHAPE,
     POLY_SHAPE,
     NUM_SHAPES
 } ShapeType;

 typedef struct ShapeMassInfo {
     double mass;
     double moment;
     Vector com;
     double area;
 } ShapeMassInfo;

 typedef BB (*ShapeCacheDataImpl)(Shape *shape, Transform transform);
 typedef void (*ShapeDestroyImpl)(Shape *shape);

 typedef struct ShapeClass {
     ShapeType type;

     ShapeCacheDataImpl cacheData; // BB cache data
     ShapeDestroyImpl destroy; // Destroy function
 } ShapeClass;

// General structure for shapes.
 struct Shape {
     const ShapeClass *klass;

     Space *space;
     Body *body;
     ShapeMassInfo massInfo;
     BB bb;

     // bool sensor; // do we even need this
     // NOTE: As a result of this, the corresponding sensor functions have
     // NOT been implemented.

     double elast;
     double fric;
     Vector surfaceVel;

     CollisionType type; // NOTE: Has been implemented, but not sure if needed.
     // ShapeFilter filter; // same here idk what this is all about yet

     Shape *next;
     Shape *prev;

     HashValue hashId;
 };

 // Struct for circle shapes.
 typedef struct CircleShape {
     Shape shape;

     Vector center; // Local coordinates
     Vector tcenter; // World coordinates

     double radius;
 } CircleShape;

 // Struct for segment shapes.
 typedef struct SegmentShape {
     Shape shape;

     Vector a; // First endpoint
     Vector b; // Second endpoint
     Vector n; // Normal

     Vector ta; // World first endpoint
     Vector tb; // World second endpoint
     Vector tn; // World normal

     double radius; // 'Thickness'

     Vector aTangent;
     Vector bTangent;
 } SegmentShape;

 #define POLY_SHAPE_INLINE_ALLOC 6

 typedef struct SplittingPlane {
     Vector v0;
     Vector n;
 } SplittingPlane;

 // Struct for polygon shapes.
 typedef struct PolyShape {
     Shape shape;

     double radius;
     int count;
     // The untransformed planes are appended at the end of the transformed planes.
     SplittingPlane *planes;

     // Allocate a small number of splitting planes internally for simple poly.
     SplittingPlane _planes[2*POLY_SHAPE_INLINE_ALLOC];
 } PolyShape;

 Shape *shapeInit(Shape *shape, const ShapeClass *klass, Body *body, ShapeMassInfo massInfo);

#define BODY_FOREACH_SHAPE(body, var)\
    for(Shape *var = body->shapeList; var; var = var->next)

#endif
