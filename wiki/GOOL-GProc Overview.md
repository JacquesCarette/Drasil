## File Structure
- `drasil-gool` exposes two modules: `GOOL.hs` and `GProc.hs`.  `GOOL.hs` exports everything that is strictly OO as well as everything that is common between OO and procedural.  Likewise, `GProc.hs` exports everything that is strictly procedural as well as everything that is common between OO and procedural.
- `Drasil/GOOl` currently contains everything GOOL/GProc related.  This may change as our restructuring continues, but is the current way things are.
	- `InterfaceCommon.hs`, `InterfaceGOOL.hs`, and `InterfaceProc.hs` define the main external interface, essentially the 'syntax' of GOOL/GProc.  This consists of a bunch of typeclasses as well as some smart constructors.  The typeclasses for each file are wrapped up in the `SharedProg`, `OOProg`, and `ProcProg` typeclasses, respectively.
	- `RendererClassesCommon.hs`, `RendererClassesOO.hs`, and `RendererClassesProc.hs` define an internal interface for rendering `Doc`s.  The typeclasses of each file are wrapped up in `CommonRenderSym`, `OORenderSym`, and `ProcRenderSym`, respectively.
	- `CodeInfoOO.hs` and `CodeInfoProc.hs` are preprocessors which give the actual language renderers some extra information.  They implement `OOProg` and `ProcProg` respectively, but not `OORenderSym` or `ProcRenderSym`.
	- `CodeType.hs` defines the types that values can have.
	- `Helpers.hs` provides some miscellaneous functions that are helpful in GOOL.  Most of them have to do either with monads in general or with the state monad.
	- `LanguageRenderer.hs` gives some functions that are used by most renderers in their implementations.
	- `AST.hs` defines what information is kept about various GOOL/GProc constructs.  Examples include the type of a value and the name of a variable.  This is helpful for when you are given a value and need to know something about it.
	- `State.hs` gives the `GOOLState`, which keeps track of everything that needs to be kept track of in GOOL/GProc.  For example. the AST needs to be kept in the `GOOLState` so that we can get information back out of GOOL/GProc constructs later.
	- There are a few others that you might come across, but these are the main ones.
- `Drasil/GOOL/LanguageRenderer` contains the language renderers, as well as modules containing function implementations that are common between multiple renderers.
	- `LanguagePolymorphic.hs` contains function implementations that are shared between all renderers.
	- `CLike.hs` contains function implementations for features that follow the 'classic C-like' structure.
	- `CommonPseudoOO.hs` contains function implementations where not all languages use that implementation, but there's no other pattern between them.
	- `AbstractProc.hs` contains function implementations that are only used by procedural languages.
	- `Macros.hs` gives implementations for functions that are just syntactic sugar - e.g. in Java, C#, and C++, a `listSlice` translates to a `for`-loop.
	- `Constructors.hs` gives more technical helper functions that have to do with creating values.