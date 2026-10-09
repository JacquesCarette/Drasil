# MATLAB Code Generation in Drasil: Work Record

Xinlu Yan · May–August 2026

---

## What Drasil does

Drasil takes scientific knowledge such as equations, definitions, and constraints. Encoded once, it generates multiple artifacts: requirements docs, design docs, and code. The code generator is called GOOL. Before this summer it could generate six languages: Java, C++, C#, Python, Swift (through an object-oriented path) and Julia (through a procedural path called GProc).

This summer I added MATLAB as a second procedural backend, making it the seventh language.

---

## What I did

### 1. Built a MATLAB renderer

GOOL works like this: it has an internal representation of code ("declare a variable," "write a loop," "call a function"), and each language has a **renderer** that translates these into concrete syntax. A renderer is a Haskell file full of typeclass instances: one instance per kind of syntax construct.

My job was to write the MATLAB renderer (`MatlabRenderer.hs`). This means implementing every instance so that GOOL's internal representation comes out as valid `.m` files.

### 2. Handled MATLAB's differences from the other six languages

**Starting from `undefined`.**

The skeleton renderer was cloned from Julia, and I spent about a week on that before backing out. The problem was that the clone compiled: any method I had not adapted yet still emitted Julia syntax, and nothing flagged which ones I had not read. So I replaced every body with `undefined` and worked from the crashes: run Projectile, take the method name off the stack trace, implement it, run again. That way the todo list came from the compiler rather than from my own notes, and I knew I was done when nothing crashed.

Two things decided where each difference got handled.

One is whether the difference is only a matter of spelling. If MATLAB writes the same thing differently, `MatlabRenderer.hs` absorbs it and the shared layer never learns MATLAB exists. Touching the shared interface is only justified when GOOL has already thrown the information away, the way an emitted loop no longer says "add two vectors". `NativeVector` in section 5 is the one place that happened.

The other is whether a MATLAB programmer would write it that way. Generated code gets read by people, so when two encodings both run correctly I took the idiomatic one, even where it cost the renderer an extra type test or a special case.

#### Functions and program structure

**Return values are assigned to a named output variable.**
MATLAB puts the result name in the header, as in `function [r] = f(x)`, and has no `return expr`. All of it fits inside `returnStmt`, which assigns to a variable called `result`, with `intFunc` and `inOutFunc` writing the matching header and `multiReturn` producing `[a, b] = f(x)`. I kept it there because the value being returned does not change, only the place it gets written down, so there was no reason to make six other backends carry a "named output" concept.

**The module gets renamed after its first function.**
MATLAB requires a file to be named after its top-level function, and Drasil modules have no such rule. I could have asked each case study to pick module names that satisfy MATLAB, but then a language-agnostic example would be encoding a filesystem rule for a backend it does not know about. `buildModule` does the renaming when there is no explicit main.

Parameters and lambdas are mechanical. `mlParam` prints the name and drops the type, since MATLAB annotates neither, and `mlLambda` emits `@(x) expr`.

#### Declarations, types, and constants

**A bare declaration gets a default value.**
MATLAB has no declaration separate from assignment, so the obvious move is to render `varDec` as nothing at all. That breaks any program that declares first and assigns inside a branch: on the paths that skip the assignment the variable never exists, and MATLAB complains at the first read, some distance from the real mistake. Desugaring to `varDecDef` with a zero or empty value keeps what a declaration promises, which is that the name is defined from here on. `constDecDef` becomes a plain assignment for the same reason, MATLAB having no `const`.

Types never reach the output at all. The renderer needs `CodeType` to choose format strings, cell wrapping and casts, but it never prints a type name. `arrayType = listType`, because MATLAB does not distinguish them and inventing a distinction would give the same array two spellings.

#### Operators and expressions

Some of this is substitution: `~` for `!`, `~=` for `!=`, `mod` as a function call at multiplication precedence. Two cases needed a decision.

**String equality is chosen by type at render time.**
`==` compares char by char in MATLAB, so it is wrong for strings. `isequal(a, b)` would have been correct for every type and would have saved the type test, but it also wraps ordinary numeric comparisons in a function call no MATLAB programmer writes. `CodeType` is available right where the operator is rendered, so `mlEqOp` looks at it and emits `strcmp(a, b)` for strings, `==`/`~=` otherwise.

**Ternary becomes arithmetic.**
MATLAB has no `cond ? a : b`, and the faithful translation would be an `if` statement. The problem is that a ternary sits inside a larger expression, and the expression renderer has no way to emit statements around it. So it comes out as `(cond) .* (a) + ~(cond) .* (b)`, which stays an expression and works element-wise. Both branches get evaluated, which is safe here because GOOL's ternary picks between pure values, though it is worth recording in case that stops being true.

#### Control flow

Blocks close with `end`, `else if` is one word, `throw` becomes `error('message')`, and try/catch becomes `try ... catch e ... end`. Two constructs needed more than that.

**A C-style `for` becomes a `while`.**
MATLAB's `for` only walks a range, while GOOL's carries an arbitrary init, condition and update. I could have recognized the range-shaped cases and emitted `start:step:stop` for them, but a pattern match like that fails open: any loop it does not recognize breaks quietly. `init; while cond; body; update; end` handles every case under one rule. GOOL's range-based `for` really is a range, so that one still maps onto `start:step:stop`.

**`switch` is desugared even though MATLAB has one.**
MATLAB's `switch` does not fall through, and it matches cases differently from the C-family languages GOOL's `switch` was modeled on. Mapping it directly would be right in the common case and quietly wrong at the edges, and nobody reads generated code closely enough to catch that. `switchAsIf` already does this for other renderers and turns it into an `if`/`elseif` chain.

#### Indexing and containers

**Constant index arithmetic folds in the renderer.**
GOOL counts from 0 and MATLAB from 1, so `intToIndex` adds one. Printing `(0 + 1)` would be correct, and MATLAB would fold it anyway, but anyone reading the generated file trips over it every time, so index 0 comes out as `(1)`. The `listLast` helper has the same motivation: it removes the `- 1 + 1` that appears when a length-based index passes through both conventions.

**String lists are cell arrays.**
MATLAB packs equal-length text into char matrices and needs `{}` for anything ragged, so a list of strings cannot be an ordinary array. The `string` type would fit better but rules out older MATLAB and Octave, which the generated code should still run under. `mlCellWrap` checks the element type at each access and picks `{}` or `()`, and `mlListDec` initializes to `{}` or `[]` to match.

The operations MATLAB does not have at all are written out by hand. There is no list insert, so `mlListAdd` splices: `[lst(1:i-1), val, lst(i:end)]`. There is no set type either, so set operations run over ordinary arrays through `ismember`, `union` and `setdiff`. `containers.Map` would give real set semantics at the cost of turning a printable, comparable value into an object with key bookkeeping, which is more machinery than GOOL's small collections need. `notNull` becomes `~isempty(v)`, and `indexOf` is `find(lst == val, 1) - 1`.

#### Printing and I/O

**Everything prints through `fprintf`, console included.**
MATLAB has no generic `print(x)`. `disp` is the obvious choice for the console, but it takes no format, adds its own newline, and needs a different call for files, so the code would fork three ways. With `fprintf` there is one path: the file handle is an optional first argument, and println is a `\n` in the format string rather than a second implementation. `mlPrint` picks `%s`, `%c` or `%g` from the type.

Input is read as text and converted afterwards, for the same reason. `input('', 's')` from the console and `fgetl` from a file both return text, so `mlInput` reads text whatever the target type is and wraps numbers in `str2double`. Reading a whole file loops on `ischar`, which is how MATLAB signals end of file.

#### Type casting

MATLAB has no cast syntax like `(int)x`, and its conversion functions have nothing in common with each other: `str2double`, `num2str`, `char`, and `logical(str2double(...))` for string to boolean. With no general helper to build on, `mlCast` matches on the source and target types together and emits the right function for the pair. A cast between identical types disappears, so a redundant cast in GOOL leaves no trace.

#### Things that don't exist in MATLAB

- **Imports produce nothing.** MATLAB finds functions by filename on the path, so `langImport` and `modImport` emit empty output. Raising an error instead would reject programs that are perfectly valid MATLAB, since the import really is satisfied, just not by a statement.
- **Only line comments.** `%{ %}` exists, but the markers have to sit alone on their own lines, which conflicts with how GOOL indents doc comments. `%` on every line is uglier but has no such constraint.
- **Command-line arguments arrive in `varargin`.** The entry function takes `varargin`, and `arg n` renders as `varargin{n+1}`, cell-indexed and 1-based, so the index conversion applies here too.

### 3. Got Projectile running

By mid-June, the Projectile case study could generate MATLAB and produce output numerically identical to the Python and Julia versions. This was the first proof that the renderer actually works end-to-end.

After that I spent the rest of the summer filling in the remaining methods and integrating MATLAB into the shared procedural test suite so any future breakage shows up automatically.

### 4. Found a deeper problem: GOOL can't express vector operations

While working on the renderer I noticed something: GOOL has no concept of "vector." Before any renderer sees the code, `spaceToCodeType` converts `Vect` into `List`. So a vector sum becomes an indexed loop:

```
for i = 1:length(x)
    y(i) = x(i) + z(i);
end
```

In Java or C++ this is fine: they don't have native vector arithmetic anyway. But in MATLAB this is both ugly and slow. MATLAB programmers write `y = x + z`, and the loop version has real performance overhead because of per-iteration interpreter cost.

The renderer can't fix this on its own. By the time it sees the code, the vector addition is already a loop: the intent is lost.

### 5. Designed and implemented NativeVector

To fix this I added a typeclass called `NativeVector` to GOOL's shared interface (`InterfaceCommon.hs`). It has six operations:

- `vecAdd`: add two vectors
- `vecScale`: multiply a vector by a scalar
- `vecDot`: dot product
- `vecMag`: magnitude (norm)
- `vecUnit`: unit vector
- `vecIndex`: get an element

It also has `vecType` and `litVec`, which default to the language's ordinary list type and list literal, so a backend only overrides them if it represents vectors some other way.

The renderer now receives "add these two vectors" instead of "loop and add elements", so MATLAB can emit `y = x + z`.

Every operation takes and returns a value rather than a statement, which is what lets them nest: `vecAdd (vecScale s a) b` comes out as `2.0 * a + b` on one line, where a statement-based version would need a temporary per step. For the same reason there is no `vecSub`. The translator writes subtraction as `vecAdd x (vecScale (-1) y)`, so backends have one less method to implement.

**Decoupled from ProcProg.** `NativeVector` lives in the shared layer, not the procedural-specific layer. A language opts in by implementing the typeclass, and languages without native vectors, like Java, just don't. Putting it in the procedural layer would have tied vector support to a generation path instead of to the language itself.

**Wiring it in was a separate piece of work**. The class on its own changes no output, because Drasil's expression translator was still routing vector expressions through the generic list helpers that produced the loops. Each case in `convExprProc` had to be pointed at the new operations, and the `NativeVector` constraint added to about ten signatures along the way.

**Validated against Julia.** The MATLAB instance shipped with the class itself, while Julia's sat as six `undefined` bodies. The interface was already merged by the time I filled them in, so if I had baked MATLAB assumptions into it, Julia would have forced a change to the class. It didn't. That work touched the Julia renderer, the test and a new golden file, and left `InterfaceCommon.hs` alone.

Julia did need different machinery in places. `dot` and `norm` are builtins in MATLAB but live in Julia's `LinearAlgebra` library, so one instance uses `funcApp` and the other `libFuncApp`, which also brings in an import. `vecIndex` needed a helper in MATLAB to fold the index conversion, while Julia reused its existing list access. If the class had been written in terms of emitting a builtin call, neither would have fit.

Both backends have a committed golden file (`VectorTest.hs`), so a regression in either shows up as a test failure.

---

## Binary Star System (BSS) case study

This is a separate contribution from the MATLAB work. I built BSS as a new Drasil case study during the winter term (CAS741) and merged it this summer.

### What it is

BSS models a two-body gravitational system: two stars orbiting each other under Newtonian gravity. Given the masses, initial positions, and initial velocities of both stars, it solves an ODE system to compute their trajectories over time.

### What it generates

From the encoded knowledge, Drasil generates:

- **An SRS document** (in HTML, PDF, Jupyter, and mdBook) with all the standard sections: problem description, assumptions, theoretical models, instance models, data constraints, requirements, traceability matrices.
- **Runnable code** in Python, Java, C++, and C#.

---

## What's left

**Struct support in GProc: unlocking Modular Program mode.**

This is the biggest outstanding item. Currently, when `Choices` is set to `Modular Program`, the code generator bundles input variables into an `InputParameters` container. In the OO path this container is a class: the generated Java/Python code does `inParams = new InputParameters(); inParams.v_launch = ...`. The procedural path has no equivalent, so it throws errors.

To fix this, the procedural path needs structs as a replacement for classes:

1. **GOOL layer**: add struct definition and field access to the procedural interface. MATLAB has `s.field = value`; Julia has `mutable struct S ... end`.
2. **`Import.hs`**: replace the expression-level errors with struct-based implementations: field access becomes `s.field` instead of `obj.field`, construction becomes a struct literal instead of `new Class()`.
3. **`Modules.hs`**: `genInputModProc` and `getInputDeclProc` need to generate struct declarations and initialization.

This work intersects with Brandon's typeclass refactor of GOOL's type hierarchy. The struct interface should be built on top of the new structure, not the old one, so coordination is needed.

**More case studies.** Only Projectile generates MATLAB so far. GlassBR would be the right next target: it uses lists and constraints much more heavily and would be a real test of whether the renderer is actually complete or just complete enough for one example.