The traceability and dependence between knowledge currently in Drasil is not always explicit.  This makes it difficult to systematically analyze and reuse the existing information.  We don't currently explicitly know if an existing theory can be reused in another problem.  For instance, the equations for projectile motion only mention the translation of the body, not its orientation.  The fact that orientation is out of scope is not explicit in the current version of Projectile, so someone could naively use the existing theories inappropriately. A more formal version of the theories will also help with codifying the knowledge in the Drasil DSLs. 

Part of the problem is that the connection between the theories currently mostly depends on the author writing text to make the connection.  If we made the theories more formal, then the connection between them could be enforced.  For instance, the Drasil compiler could complain if a theory is used in a context where one of the assumptions on which it is based is not valid.  For instance, an equation assuming constant acceleration is not valid when the acceleration varies with time. 

Although more formality is desirable, we still desire the flexibility to use informal arguments when needed for practical purposes.  For instance, the author of a requirements document can usually assume that the reader is familiar with basic mathematical concepts, like real arithmetic, trigonometry, differentiation, vectors etc. Rather than have to define the details of these basic building blocks, the author should be should be able to assume that  (flexiformal [(Iancu, 2017)](https://d-nb.info/1141379643/34)).

Methodology - going through the projectile example (link).  Redoing sections.  Highlight major changes.

...

# Addition of Types

Type information is valuable for making the document unambiguous.  Type information also improve our confidence that the generated code is correct.  In the projectile example the relevant types are Real, Real^2 (for 2D vectors) and Real^3 (for 3D vectors).  There is also a string type for the final output of the program.  A type "time" is introduced as a synonym for Real.  The time type helps the documentation be clearer.
 
# Assumptions and Decisions

## Scope Decisions

Scope decisions are the decisions on what theories to leave out.  For instance, for the projectile problem, the theories related to rotational motion and kinetics (forces and motion) are left out.  This scope decision is the reason that the projectile problem can be solved as a kinematics problem for a projectile without worrying about its rotation.  If the scope decisions are not documented, then the reader will not know what has been excluded.  This is analogous to a lie of omission. The scope decisions will have to be justified, possibly by adding additional assumptions, such as the assumption that air drag can be neglected. 

The scope decisions will not be referenced by (invoked by) anything, other than the section of the document that covers the scope of the requirements.

## Modelling Decisions

## Background theory assumptions

## Helper theory assumptions

## Generic theory assumptions

## Specific theory assumptions

## Rationale assumptions

# Changes to Theories

## Potential components of a theory ...

Refinement.  Rationale for constraints.  Inherit or justify assumptions.

### Refname

The name that is used to refer to the theory, like TM:acceleration in the Projectile example.  The name of the theory is used for referring to the theory.  The name provides the id that is used for traceability.

### Label

A short name for the theory.  The label can be longer than the Refname.

### Source

An external reference, or references, that provides information on the theory. 

### RefBy

The other theories or requirements that reference this theory.  A theory references another theory if it is used in the refinement of that theory.  A requirement uses a theory if the requirement points to the theory for details on what the requirement means.

### Equation

### Description

### Constraints

### Notes

### Detailed Derivation of This Theory

### Context Theories Used by This Theory

The context theories that are needed for someone to understand the current theory.  These theories do not have to be explicitly invoked in the  notes or the detailed derivation.  They can just be attached to a theory because the author of the theory knows they are necessary.  The context theories are a user input, although Drasil should check that any context theories that are explicitly used in the notes or detailed derivation are included in the list.

### Initial Theories Used by This Theory

The initial theories are the theories that are refined to create the new theory.  These theories have to be explicitly invoked in the notes or detailed derivation of the theory.  The list of initial theories should automatically be created by Drasil.  This list is not a user input.

### Preconditions for This Theory

These are the assumptions and modelling decisions that need to be satisfied to use the theory.  All assumptions and modelling decisions have to be made explicit in the notes and detailed derivation.  The list of preconditions should be automatically created by Drasil.  This list is not a user input.

By default theories inherit all the modelling decisions and assumptions from the theories they refine.  However, in some cases an assumption will need to be overridden an replaced with a new assumption or assumptions.  For instance, in projectile motion, the assumption of 1D motion for the GD:rectVel is overridden by the assumption of 2D for GD:velVec.  When an assumption is overridden, justification needs to be provided by the author.

### Units

### Input

### Output

### Input Constraints

### Output Constraints

## Context Theories

In science and engineering the theories that are used to solve practical problems are built on a foundation of mathematical and physical knowledge. For practical reasons, it is not usually feasible to define all of the details down to the most fundamental level. For instance, the audience reading the requirements will typically know the concepts of real arithmetic, differentiation, trigonometry, etc.  Nevertheless it is helpful to make the dependence on fundamental theories explicit, even if those theories themselves are not explicitly defined. These theories form the context in which the higher-level theories exist.

Examples: 

## Background Theories

Examples: 

## Helper Theories

Examples:

## Generic Theories

Not specialized to a specific problem.  Likely reusable across many problems.

Examples:

## Problem Specific Theories

Examples:

## Final Theories

step before refinement to code.  Mentioned explicitly by requirements.

Examples: