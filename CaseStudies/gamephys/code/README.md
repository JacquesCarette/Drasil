Chipmunk Physics Simulator
===========================

Mapping of Modules and Code Files
---------------------------------

Refer to the Module Guide (MG) in the documentation directory for more details on each module.

- **M1 - Hardware-Hiding Module**: C (implemented by the programming language)
- **M2 - Rigid Body Module**: Body_private.h, Body.h, Body.c
- **M3 - Shape Module**: Shape_private.h, Shape.h, Shape.c
    * **Circle Module**: CircleShape.h, CircleShape.c
    * **Segment Module**: SegmentShape.h, SegmentShape.c
    * **Polygon Module**: PolyShape.h, PolyShape.c
- **M4 - Space Module**: Space_private.h, Space.h, Space.c, SpaceStep.c
- **M5 - Arbiter Module**: Arbiter_private.h, Arbiter.h, Arbiter.c
- **M6 - Control Module**: Chipmunk.h, Chipmunk_types.h, Chipmunk.c
- **M7 - Vector Module**: Vector.h
- **M8 - Bounding Box Module**: BB.h
- **M9 - Transform Matrix Module**: Transform.h
- **M10 - Spatial Index Module**: SpatialIndex_private.h, SpatialIndex.h, SpatialIndex.c
- **M11 - Collision Solver Module**: Collision.h, Collision.c
- **M12 - Sequence Data Structure Module**: Array_private.h, Array.c
- **M13 - Linked Data Structure Module**: BBTree_private.h, BBTree.c
- **M14 - Associative Data Structure Module**: HashSet_private.h, HashSet.c

Revision History
----------------

**29/07/2016**
- Moved kinematic collision-related functions from Chipmunk.h to Collision.h. Updated module mapping accordingly.
- Moved Transform definition from Chipmunk_types.h to Transform.h.
- Other minor revisions as I write the Module Interface Specifications.

**13/07/2016**
- Edited Makefile's system-specific variables for more cross-compatibility.
- Updated module mapping based on revised Module Guide.

**11/07/2016**
- Removed lib directory and modified Makefile. Makefile will now automatically create libraries (the directory and the file) upon running make.

**08/07/2016**
- Modified Chipmunk source code to accommodate unit tests. Normally, invalid inputs will cause Chipmunk's built-in assertions to exit the program, which breaks tests. In 'Unit Test' mode, Chipmunk's built-in assertions will not exit the program, but rather return an error value which can be checked with assertions. To toggle this mode, add '#define UNIT_TEST' to a test file.
- Modified Makefile so that the default 'make' instruction is 'make prog'. Added 'make all' to build everything.
- Also modified 'make test' - the screen and results file will only display test results (printed to stdout). All Chipmunk warnings caused by built-in assertions (printed to stderr) are suppressed.
- Added unit tests for BB.h.
- Added mapping between modules and files for Chipmunk (see above).

**30/06/2016**
- Changed "Chipmunk.h" to "Chipmunk_types.h" and "Private.h" to "Chipmunk.h". "Chipmunk.h" is now the main header file for this library.
- Added README files to every Chipmunk directory.
- Added Makefile. This will build the static library, sample program executable and unit tests.

**28/06/2016**
- Moved example program Main.c to the 'example' folder
- Added unit tests for Vector.h in 'tests'

**27/06/2016**
- Separated data structures into private header files in Private.h.
- Reorganized code.
- Added user-defined assertions.

**23/06/2016**
- Bug in test program Main.c fixed. Now working on code cleanup.

**22/06/2016**
- Need to place disclaimer!
- Included most necessary modules.
- Need to clean up unused code, create user-defined assertions and organize data structures (currently most things are placed in Data.h).
- 30s test simulation (based off Chipmunk's example program) runs fine for 21s and then segfaults. Will look into this. Valgrind error log has been included for reference.
