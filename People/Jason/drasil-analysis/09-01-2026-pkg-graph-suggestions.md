## Discussion Preliminary Steps Found in Shallow File Analysis

**Objectives**: For display in the GHRC paper:

1. Generate more code artifacts (more generation options in the code generator).
2. Explainable `-code` internals.

**Method**: Shallow file analysis to understand state of the code, and gradually revising until the code is explainable and can generate more variants.

**Progress**: We've done a shallow file analysis on `gen`, `code`, `printers`, `system`, and `makefile`, and have been making select changes. The next subsection will discuss the current state and the following ones will propose changes that target the package dependency graph to gradually improve explainability of the codebase more broadly.

### 0. Current State

```mermaid
flowchart LR
    projectile --> gen
    gen --> code & data
    data --> theory
    code --> srs & printers & gool
    printers --> theory
    srs --> system & theory
    theory --> metadata
    system --> lang
    metadata --> lang
    lang --> database
```

These are Drasil's essential packages. We've done a shallow file analysis on `gen`, `code`, `printers`, `system`, and `database`.

Current understanding of packages:

1. **`database`**: Foundational package that establishes a definition of a "chunk" (reusable information with a `UID` and references to [dependencies on] other chunks), along with the in-memory typed database (`ChunkDB`) and lookup and reference machinery.
2. **`lang`**: The core knowledge representation DSL and foundational chunk typeclasses. Defines mathematical expressions (`Expr`, `ModelExpr`, `Literal`), types (`Space`), physical units (`UnitDefn`), symbols (`Symbol`), natural language sentences (`Sentence`), base document ASTs (`Document`), and chunk classes and types (`Quantity`, `Concept`, `Constrained`, etc. and `DefinedQuantityDict`, `ConstrConcept`, `UnitDefn`, etc.).
3. **`metadata`**: Shared Drasil-level concepts, including relevant papers the SRS references, people that contributed to said papers, domain-specific definitions, and standard concepts (software terminology, math concepts, and Drasil metadata).
4. **`system`**: Defines `SystemMeta`, binding top-level contextual project descriptors (via `HasSystemMeta`, system name, authors, purpose, scope, background, motivation) to a knowledge base (`ChunkDB`), intended to be carried by "projects" (which are intended to represent choice-free core components of a high-level representation of a specific kind of software project).
5. **`theory`**: Scientific theory abstractions and mathematical model schemas (`ModelKinds`). Also contains pretty displayable variants (`DataDefinition`, `GenDefn`, `TheoryModel`, and `InstanceModel`), along with derivations and constraint sets.
6. **`data`**: Reusable scientific ontology and physical knowledge repository. Contains standard physical quantities, SI units, physical constants, and foundational physical/mathematical theories (kinematics, thermodynamics, etc.).
7. **`printers`**: Document renderers and backend format printers. Translates Drasil's internal document ASTs, expressions, layout objects, and citations into concrete target formats (LaTeX/PDF, HTML with CSS, Markdown, JSON, and plain text).
8. **`srs`**: Software Requirements Specification layout DSL and schema compiler (based on the Smith et al. framework). Compiles declarative document descriptions (`DocDecl`) into generic `Document` ASTs, erasing as many chunks as possible along the way.
9. **`gool`**: A language-polymorphic embedded DSL abstracting imperative, object-oriented, and procedural programming languages, with target-specific printers generating (locally, not architecturally) idiomatic code in Python, Java, C++, C#, Swift, Julia, and MATLAB.
10. **`code`**: ICO program code generator. Transforms scientific models (`srs` & `theory`) and architectural/implementation design decisions (`Choices`) into imperative program specifications (`CodeSpec`), linking numerical ODE solvers and generating code (via GOOL/GProc), build scripts (Makefiles), and READMEs.
11. **`gen`**: A facade! High-level APIs for building various kinds of software artifacts (currently only the SRS and SRS+Code and SRS+CodeZoo showcases), but actively being reworked. Currently also exposes "common knowledge" intended to be used by all examples. The 'best' version of those only wraps artbitrary systems with an extra generator that exposes options for writing Drasil debugging data (from the `ChunkDB`).
12. **`projectile`**: (1) an executable that exercises one of the off-the-shelf generators declared in `gen` and (2) a library containing the "projectile" problem encoding along with generation options for the executable to use.

### 1. `gen --> code & data`

`drasil-gen` depends on `-srs`, `-code`, and `-data` directly because it wraps up the file generators for the SRS generator and the code generator. It also contains the "basis `ChunkDB`" that is intended to be used by all our existing projects.

```mermaid
flowchart LR
    projectile --> gen & code & data
    gen --> system
    data --> theory
    code --> srs & printers & gool
    printers --> theory
    srs --> system & theory
    theory --> metadata
    system --> lang
    metadata --> lang
    lang --> database
```

### 2. `code --> srs`

Create an `srs-extract` project

```mermaid
flowchart LR
    projectile --> gen & srs2code & data
    srs2code --> srs & code
    gen --> system
    data --> theory
    code --> system & printers & gool
    printers --> theory
    srs --> system & theory
    theory --> metadata
    system --> lang
    metadata --> lang
    lang --> database
```

### 3. Drasil generates artifact showcases!

Generated artifacts create showcases of artifacts

```mermaid
flowchart LR
    projectile --> gen & showcase & data
    showcase --> srs2code
    srs2code --> srs & code
    gen --> system
    data --> theory
    code --> system & printers & gool
    printers --> theory
    srs --> system & theory
    theory --> metadata
    system --> lang
    metadata --> lang
    lang --> database
```

### 4. `drasil-lang` contains code that belongs in `-code`, but dependency cycle!

#### 4.1. Move `-printers` down

```mermaid
flowchart LR
    projectile --> gen & showcase & data
    showcase --> srs2code
    srs2code --> srs & code
    gen --> system
    data --> theory
    code --> system & gool
    srs --> system & theory
    theory --> metadata
    system --> lang
    metadata --> lang
    lang --> printers & database
    printers --> database
```

#### 4.2. Move `-code` code in `-lang` up to `-code`

No change.

### 5. Documents, documents, documents

#### 5.1. Building

```mermaid
flowchart LR
    projectile --> gen & showcase & data
    showcase --> srs2code
    srs2code --> srs & code
    gen --> system
    data --> theory
    code --> system & gool
    srs --> system & theory & document-building
    theory --> metadata
    system --> lang
    metadata --> lang
    lang --> printers
    printers --> database
    document-building --> printers
```

#### 5.2. Rendering

```mermaid
flowchart LR
    projectile --> gen & showcase & data
    showcase --> srs2code
    srs2code --> srs & code
    gen --> system
    data --> theory
    code --> gool
    srs --> system & theory & document-building
    theory --> metadata
    system --> lang
    metadata --> lang
    lang --> document-rendering
    document-building --> document-rendering & database
```

### 6. Splitting up `-lang`

```mermaid
flowchart LR
    projectile --> gen & showcase & data
    showcase --> srs2code
    srs2code --> srs & code
    gen --> system
    data --> theory
    code --> system & gool
    srs --> system & theory & document-building
    theory --> metadata
    system --> nat-lang
    metadata --> math & people & sources
    sources --> people & nat-lang
    nat-lang --> document-rendering
    people --> document-rendering
    math --> document-rendering
    document-building --> document-rendering & database
```
