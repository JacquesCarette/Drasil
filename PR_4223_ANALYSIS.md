# PR 4223: Vector Integration in DblPend Example - Comprehensive Analysis

## Overview
**Title:** Vector integration in the DblPend example  
**Author:** @sarrasoussia  
**Status:** Open (targeting branch `CSchankGA`, re-targeted from `main`)  
**Scale:** 414 files changed (+6,206 additions, -19,554 deletions)

## Background and Context

### Problem Statement
This PR aims to integrate vector operations directly into the Double Pendulum (DblPend) example in Drasil, moving from component-based representations to vector-based formulations using Clifford algebra.

### Discussion History
1. **Initial confusion** (Sept 2025): Team discussed whether this PR was superseded by #4261
2. **Review request** (Oct 2025): @sarrasoussia marked as ready for review
3. **Reviewer feedback** (Oct 2025): @smiths provided detailed SRS markup with concerns about Clifford algebra appearing in external documentation
4. **Branch retargeting** (Nov 2025): @balacij retargeted from `main` to `CSchankGA` (#4261) to see actual diff

### Key Insight from Reviews
**Critical feedback from @smiths:** "Clifford algebra should not show up at all in the SRS. Clifford algebra is internal to Drasil. Externally, we want to generate documentation that looks like typical vector algebra."

## Major Changes by Category

### 1. Core Infrastructure Changes

#### A. Space Type System Evolution (`ClifS` - Clifford Space)
**Files affected:**
- `code/drasil-code/lib/Language/Drasil/Code/Code.hs`
- `code/drasil-code/lib/Language/Drasil/Chunk/Parameter.hs`

**What changed:**
- Replaced `Vect` space type with `ClifS` (Clifford Space) representation
- `ClifS` structure: `ClifS Dimension ClifKind BaseSpace`
  - Dimension: Fixed n, or VDim "n" for symbolic
  - ClifKind: Scalar, Vector, Bivector, Multivector
  - BaseSpace: Real, Integer, etc.

**Code generation mapping:**
```haskell
spaceToCodeType (S.ClifS _ kind s) = case kind of
    S.Scalar      -> spaceToCodeType s
    S.Vector      -> map List (spaceToCodeType s)
    S.Bivector    -> map List (spaceToCodeType s)
    S.Multivector -> map (List . List) (spaceToCodeType s)
```

#### B. Expression System Updates
**Files affected:**
- `code/drasil-code/lib/Language/Drasil/Code/Imperative/Import.hs`
- `Drasil.Code.CodeExpr.Development` (referenced)

**What changed:**
- Renamed vector operations to Clifford operations:
  - `UnaryOpVV` → `UnaryOpCC` (Clifford to Clifford)
  - `UnaryOpVN` → `UnaryOpCN` (Clifford to Number)
  - `VVVBinaryOp` → `CCCBinaryOp`
  - `VVNBinaryOp` → `CCNBinaryOp`
  - `NVVBinaryOp` → `NCCBinaryOp`

- Added new unary operations:
  ```haskell
  unopCN :: UFuncCN -> (SValue r -> SValue r)
  unopCN Dim = listSize
  unopCN Norm = error "Norm operation not yet implemented"
  unopCN Grade = error "Grade operation not yet implemented"
  ```

- Binary operations now include:
  - `CAdd`, `CSub` for vector addition/subtraction
  - `WedgeProd`, `GeometricProd` (not yet implemented)
  - `Scale` for scalar-vector multiplication

#### C. ODE Library Integration
**Files affected:**
- `code/drasil-code/lib/Data/Drasil/ExternalLibraries/ODELibraries.hs`

**What changed:**
- Introduced symbolic dimension `dim :: String = "n"` for generic ODE variables
- Updated odeint library to use `ClifS (VDim dim) Vector Real` instead of `Vect Real`
- Modified function signatures for ODE system representation
- Added `modifiedODESyst` to handle new Clifford expression types

### 2. Double Pendulum Example Changes

#### A. New Vector Quantities
**Files affected:**
- `code/drasil-example/dblpend/lib/Drasil/DblPend/Unitals.hs`
- `code/drasil-example/dblpend/lib/Drasil/DblPend/Expressions.hs`
- `code/drasil-example/dblpend/lib/Drasil/DblPend/DataDefs.hs`

**New vector-based quantities introduced:**
- `posVec_1`, `posVec_2` - Position vectors for masses
- `mvVel_1`, `mvVel_2` - Velocity vectors (multivector representation)
- `mvAccel_1`, `mvAccel_2` - Acceleration vectors
- `velVecEqn_1`, `velVecEqn_2` - Velocity vector equations

**Example definition:**
```haskell
mvVel_1 = uc' \"mvVel_1\" (cn \"velocity vector of mass 1\")
          \"the velocity of mass 1 in vector form\"
          (sub (vec lV) (label \"1\"))
          (realVect (Fixed 2))  -- 2D Clifford vector space
          velU
```

#### B. Derivations Restructure
**Files affected:**
- `code/drasil-example/dblpend/lib/Drasil/DblPend/Derivations.hs`

**What changed:**
- Complete rewrite from component-based to vector-based derivations
- Header comment added: "Vector-based Derivations using Clifford Algebra"
- Removed old component derivations (velXDerivEqn, velYDerivEqn, etc.)
- New vector derivations directly use vector quantities

**Impact:** Much more concise code, but relies on Clifford algebra infrastructure

#### C. ODE System Simplification
**Files affected:**
- `code/drasil-example/dblpend/lib/Drasil/DblPend/ODEs.hs`

**What changed:**
- Massive simplification from ~40 lines of complex equations to:
```haskell
[
  sy angularVel_1,      -- d(theta1)/dt = angular velocity 1
  sy angularAccel_1,    -- d(omega1)/dt = angular acceleration 1
  sy angularVel_2,      -- d(theta2)/dt = angular velocity 2
  sy angularAccel_2     -- d(omega2)/dt = angular acceleration 2
]
```
- The complex math is now encapsulated in the definitions of angular accelerations

### 3. Data Library Updates

#### A. Physics Quantities Refactor
**Files affected:**
- `code/drasil-data/lib/Data/Drasil/Quantities/Physics.hs`
- `code/drasil-data/lib/Data/Drasil/Concepts/Physics.hs`

**What changed:**
- All vector quantities now use `realVect vecDim` instead of `Vect Real`
- Example:
  ```haskell
  -- Before
  velocity = uc CP.velocity (vec lV) Real velU
  
  -- After
  velocity = uc CP.velocity (vec lV) (realVect vecDim) velU
  ```

- Affected quantities: acceleration, displacement, force, gravitationalAccel, impulseV, momentum, moment, positionVec, tension, velocity

#### B. Math Concepts Enhancement
**Files affected:**
- `code/drasil-data/lib/Data/Drasil/Concepts/Math.hs`

**What changed:**
- Added `magnitude` concept
- Fixed concatenation operators for axis/component descriptions
- Changed from `Sentence` operators to `NPStruct` operators for proper noun phrase handling

### 4. Code Generation Infrastructure

#### A. Removed Configuration Parser
**Files removed:**
- `code/drasil-code/lib/Language/Drasil/Code/Imperative/Parsers/ConfigParser.hs`
- `code/drasil-code/lib/Language/Drasil/Code/Imperative/Parsers/README.md`

**Rationale:** Appears to be obsolete functionality not needed with new architecture

#### B. Modularization Changes
**Files affected:**
- `code/drasil-code/lib/Language/Drasil/CodeSpec.hs`
- New file: `code/drasil-code/lib/Language/Drasil/ICOSolutionSearch.hs`

**What changed:**
- Extracted `getExecOrder` function to separate module `ICOSolutionSearch`
- This function orders definitions to form execution paths from known to needed values
- Better separation of concerns

#### C. Read Input Handling
**Files affected:**
- `code/drasil-code/lib/Language/Drasil/Code/Imperative/ReadInput.hs`

**What changed:**
- Updated to handle `ClifS` types instead of `Vect`
- Modified `getDimension` to extract dimension from `ClifS (Fixed n) Vector _`
- Updated `strListAsExpr` and `strList2DAsExpr` for Clifford vectors

### 5. Example Propagation

The vector/Clifford changes propagated to other examples:
- **GamePhysics**: Updated unitals to use vector spaces
- **PDController**: Added `realVect` helper function
- **SWHS**: Added `realVect` helper function
- **SinglePendulum (SglPend)**: Similar updates

### 6. Documentation and Metadata

#### A. Generated Stable Files
**Files affected:** `code/stable/*` (many HTML, source code files)

**What changed:**
- All generated documentation now reflects the vector-based approach
- Type signatures in generated code updated
- Design logs show `ClifS` instead of `Vect`

#### B. Meeting Template
**Files affected:** `.github/ISSUE_TEMPLATE/tmpltMeetIssue.md`

**What changed:**
- Updated team member list
- Changed room from ITB/225 to ITB/112

### 7. Build System Updates

**Files affected:**
- Multiple `stack.yaml` files
- `.github/workflows/Build.yaml`, `.github/workflows/Lint.yaml`

**What changed:**
- Updated LTS resolver from `lts-22.31` to `lts-22.44`
- Updated actions/cache from `v4.2.4` to `v4.3.0`
- Updated HLint version reference in comments

## Outstanding Issues and TODOs

### From Code Comments
1. **TODO: Clifford algebra operations not yet implemented**
   ```haskell
   unopCC _ = error "Clifford algebra unary operations not yet implemented"
   ```

2. **TODO: Symbolic vector parameters in ODE**
   ```haskell
   -- TODO: Once ClifS is integrated, revisit this to allow symbolic vectors as parameter types.
   -- Likely replacement: ClifS dim Real instead of ClifS (VDim dim) Vector Real.
   ```

3. **TODO: Grade selection and multivector construction**
   ```haskell
   convExpr (NatCCBinaryOp {}) = error "NatCCBinaryOp not yet implemented"
   convExpr (Clif _ _) = error "Clif not yet implemented"
   ```

4. **TODO: Read data processing temporarily disabled**
   ```haskell
   -- TODO: Fix this for Clifford algebra - temporarily commenting out
   let bod = [] -- Empty body for now
   ```

5. **TODO: Matrix ↔ ClifS representation unclear**
   ```haskell
   -- TODO: For now, ClifS is rendered like Vect (i.e., as a list).
   -- This does not support full GA structure (e.g., blades, bivectors, matrices).
   -- Matrix <-> ClifS representation is unclear and deferred.
   ```

### From Review Comments

1. **Commented-out code** (Body.hs)
   - Reviewer: @JacquesCarette
   - Comment: "Don't leave commented-out code in files. Just delete it."

2. **Component-wise vs vector operations** (Multiple files)
   - Reviewer: @JacquesCarette
   - Comment: "Although this is correct, what we would really want here is a definition at the level of *vectors* rather than its components."

3. **Inline sentences should be extracted** (Body.hs)
   - Reviewer: @JacquesCarette
   - Comment: "These new sentences should not be inlined, but defined below and used here."

4. **Spurious formatting changes** (Body.hs, Assumptions.hs, etc.)
   - Reviewer: @JacquesCarette
   - Comment: "Try not to have spurious changes that distract from meaningful changes."

5. **Whitespace at end of lines** (GamePhysics/Unitals.hs)
   - Reviewer: @JacquesCarette
   - Comment: "Your editor is adding extra stuff at the end of some lines."

6. **Helper function placement** (PDController/Unitals.hs, SWHS/Unitals.hs)
   - Reviewer: @JacquesCarette
   - Comment: "This doesn't belong in an example, it should be abstracted out. I'd say in `drasil-lang` for now."
   - Action: Need to move `realVect` helper to `drasil-lang`

7. **ClifS exposure in external outputs** (glassbr/designLog.txt)
   - Reviewer: @JacquesCarette
   - Comment: "ClifS should not escape the insides of Drasil. Best would be if this were to say 'Vector of Length 3 of Real'"

8. **Unnecessary type constraint change** (Import.hs)
   - Changed `SharedProg` to `OOProg`
   - Author acknowledged: "That's right, unnecessary. My fault"

9. **Generated code variable name change** (glassbr ReadTable.cpp)
   - Reviewer: @JacquesCarette
   - Comment: "Does this name in the generated code really need to change?"
   - `z_vector` → `z_vect3DSor`

10. **Ungrammatical additions** (SglPend_SRS.html, index.html)
    - Added "using formulation" / "using for geometric representation"
    - Reviewer: Requested removal

## Critical Design Decisions

### 1. Internal vs External Representation
**Decision:** Clifford algebra is internal to Drasil; external docs should show typical vector algebra.

**Current State:** Partially achieved, but `ClifS` still leaks into:
- Design logs
- Type descriptions
- Generated documentation

**Required work:** Better abstraction layer to hide Clifford internals from generated outputs.

### 2. Code Generation Not Working
**Acknowledged issue:** @sarrasoussia stated "With the current changes, code generation is still not available."

**Impact:** This is a work-in-progress PR focused on getting the SRS equations correct first.

### 3. Helper Function Abstraction
**Decision needed:** Move `realVect` helper from individual examples to `drasil-lang`.

**Current state:** Duplicated in PDController, SWHS, and potentially other examples.

### 4. Dimension Handling
**Design choice:** Use symbolic dimension "n" for generic library code, allow examples to specialize.

**Example:**
```haskell
dim :: String
dim = "n"  -- Library level

-- Example level
realVect (Fixed 2)  -- 2D vector
realVect (Fixed 3)  -- 3D vector
```

## Testing and Verification Status

### What Can Be Verified
1. ✅ SRS generation (per comments, this was the focus)
2. ✅ Type checking passes (PR is buildable)
3. ✅ File structure and modularization

### What Cannot Be Verified Yet
1. ❌ Code generation (explicitly noted as not working)
2. ❌ Runtime behavior of generated code
3. ❌ Numerical accuracy of vector operations
4. ❌ Full Clifford algebra operations

## Dependencies and Related Work

### Related PRs
- **#4261 (CSchankGA branch)**: Base branch for this PR
- **#4247**: Previous discussion about this work

### External Dependencies
- LTS Stackage 22.44 (Haskell)
- GOOL (Generated Object-Oriented Language framework)
- scipy, odeint libraries for generated code

## Impact Assessment

### Positive Impacts
1. **More concise code**: ODE definitions much shorter
2. **Better abstractions**: Vector operations at proper level
3. **Clearer physics**: Matches standard vector notation
4. **Foundation for GA**: Sets up Clifford algebra infrastructure

### Challenges
1. **Incomplete implementation**: Many TODOs remain
2. **Abstraction leakage**: Clifford types visible externally
3. **Code generation broken**: Needs significant work
4. **Review feedback unaddressed**: Several cosmetic and structural issues

### Risk Areas
1. **Breaking changes**: 414 files modified, high risk of regressions
2. **Documentation inconsistency**: SRS may not match implementation
3. **Generated code quality**: Type names and structure changes
4. **Maintainability**: Complex abstraction layer with TODOs

## Recommendations for Next Steps

### High Priority
1. **Address critical review feedback**:
   - Remove commented-out code
   - Extract inline sentences
   - Remove spurious formatting changes
   - Fix grammatical issues

2. **Abstract realVect helper**:
   - Move to `drasil-lang`
   - Remove duplicates from examples

3. **Hide Clifford internals**:
   - Improve rendering of `ClifS` in generated docs
   - Show "Vector of length n of Real" instead of "ClifS ..."

4. **Fix code generation**:
   - Re-enable readDataProc
   - Implement missing Clifford operations
   - Test generated code

### Medium Priority
1. **Complete TODO items**:
   - Implement Norm, Grade operations
   - Implement WedgeProd, GeometricProd
   - Handle NatCCBinaryOp, Clif constructors

2. **Verify equations**:
   - Mathematical review of vector derivations
   - Compare with component-based versions
   - Ensure physical correctness

3. **Improve documentation**:
   - Add design documents for Clifford algebra approach
   - Document the ClifS type system
   - Explain vector space helper functions

### Low Priority
1. **Clean up stable files**:
   - Regenerate after fixes
   - Verify all examples work

2. **Code style**:
   - Remove trailing whitespace
   - Consistent formatting

3. **Test coverage**:
   - Add unit tests for vector operations
   - Integration tests for ODE solving

## Conclusion

PR #4223 represents a significant architectural shift in how Drasil handles vector quantities, moving from simple `Vect` types to a Clifford algebra-based representation (`ClifS`). While the theoretical foundation is sound and the SRS improvements are valuable, the implementation is incomplete and has several outstanding issues from code review.

### Key Achievements
- Established Clifford algebra infrastructure
- Simplified Double Pendulum equations significantly
- Updated physics quantities to use proper vector spaces
- Modularized code better (e.g., ICOSolutionSearch)

### Remaining Work
- Complete implementation of Clifford operations
- Fix code generation
- Address all review feedback
- Hide internal Clifford types from external outputs
- Move helper functions to proper locations

### Recommendation
This PR should not be merged in its current state. It requires:
1. Completion of TODOs
2. Resolution of review comments
3. Working code generation
4. Verification that equations are mathematically correct

However, the direction is promising and aligns with the goal of having Drasil work with proper vector algebra while using Clifford algebra internally.
