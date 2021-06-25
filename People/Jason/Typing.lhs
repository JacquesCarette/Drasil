\documentclass{article}

\usepackage{hyperref}

%include polycode.fmt

\begin{document}

Using the @Expr@ from the top of PR 2626's branch \href{https://github.com/JacquesCarette/Drasil/tree/658b6b84fc044d9ccb2fe86358385d81c713a75a}{658b6}, let's try to add typing.

\begin{code}
module Typing where
\end{code}


\tableofcontents

\section{Understanding our 3 Expr languages}

\subsection{Expr}
% TODO

\subsection{DisplayExpr}
% TODO

\subsection{CodeExpr}
% TODO

\section{Typing Expr}

Adding type information to @Expr@ is the goal. If we later want to add some form of typing to the @DisplayExpr@ and/or @CodeExpr@, we can, but those will likely be broken in some ways.

First, we parameterize @Expr@
\begin{spec}
data Expr a where
\end{spec}

\subsection{Primitives}
We add typing information for our ``primitives'', using @Double@ to represent @Real@s and assuming we want percentages to \textit{always} be @Real@s (potentially bad assumption, but easy fix).
\begin{spec}
  Dbl      :: Double -> Expr Double
  Int      :: Integer -> Expr Integer
  ExactDbl :: Integer -> Expr Double
  Str      :: String -> Expr String
  Perc     :: Integer -> Integer -> Expr Double
\end{spec}


\subsection{Associative Operators}

Ok, let's add typing for our associative operators
\begin{spec}
  -- Takes an associative arithmetic operator with a list of expressions.
  AssocA   :: Num a => AssocArithOper -> [Expr a] -> Expr a
  -- Takes an associative boolean operator with a list of expressions.
  AssocB   :: AssocBoolOper  -> [Expr Bool] -> Expr Bool
\end{spec}

Looks good.

\subsection{Derivatives}

@Deriv@ (Derivatives) next
\begin{spec}
  -- Derivative syntax is:
  --   Type ('Part'ial or 'Total') -> principal part of change -> with respect to
  --   For example: Deriv Part y x1 would be (dy/dx1).
  Deriv    :: DerivType -> Expr -> UID -> Expr  -- TODO?
\end{spec}

This one is very tough. With Issue \href{https://github.com/JacquesCarette/Drasil/issues/2628}{2628}, we'll be moving this into the @DisplayExpr@, so it won't be an issue for us, but assuming we wanted to add typing to it, it would be difficult. We have a few issues:

\begin{itemize}
  \item How do we deal with booleans? It appears we're severely limited here. The control flow would be fine, but I don't know what we'd do with the ``derivative of an @Expr Bool@''.
  \item How do we deal with undefinedness?
\end{itemize}

There's lots more we can explore, but let's not dig too deep. Though, the thought here might be helpful elsewhere -- namely functions.


\subsection{Chunks}

Moving along, we'll want to type our variable chunks.
\begin{spec}
  C        :: UID -> Expr a
\end{spec}


Well, this is interesting. Where does the typing come from? Smart constructors! Here's a focused example:
\begin{spec}
data Expr a where
    C :: String -> Expr a

stringyCExpr :: String -> Expr String
stringyCExpr = C

intyCExpr :: String -> Expr Int
intyCExpr = C

realyCExpr :: String -> Expr Double
realyCExpr = C
\end{spec}


I think this would be a good start, but I wonder if we would want to add typing to the actual quantities we have in Drasil. We'd then be able to pass the variables containing the quantities to C, instead of their UID.


\subsection{Arith/Bool-related Unary Operators}

\begin{spec}
  -- Unary operation for most functions (eg. sin, cos, log, etc.).
  UnaryOp       :: UFunc -> Expr Double -> Expr Double
  -- Unary operation for negation.
  UnaryOpB      :: UFuncB -> Expr Bool -> Expr Bool
\end{spec}

Okay, starting to see some problems now. 
\begin{itemize}
  \item We should split up our unary operators a bit more. Notably, the UnaryOps are always for doubles (that's not good!) -- we'll need to split it up a bit further to allow Absolute Value, Neg, etc for both Integers and Doubles. Seems like a fairly simple fix.
  \item We should also add some extra features like truncating, rounding, casting, etc -- still fairly simple additions.
\end{itemize}

\subsection{Binary Operators}

\begin{spec}
  -- Binary operator for arithmetic between expressions (fractional, power, and subtraction).
  ArithBinaryOp :: Num a => ArithBinOp -> Expr a -> Expr a -> Expr a
  -- Binary operator for boolean operators (implies, iff).
  BoolBinaryOp  :: BoolBinOp -> Expr Bool -> Expr Bool -> Expr Bool
  -- Binary operator for equality between expressions.
  EqBinaryOp    :: Eq a => EqBinOp -> Expr a -> Expr a -> Expr Bool
  -- Binary operator for ordering expressions (less than, greater than, etc.).
  OrdBinaryOp   :: Ord a => OrdBinOp -> Expr a -> Expr a -> Expr Bool
\end{spec}

Nothing out of the ordinary with @ArithBinaryOp@, @BoolBinaryOp@, @EqBinaryOp@, and @OrdBinaryOp@.

\subsection{Function calls}

\begin{spec}
  -- A function call accepts a list of parameters and a list of named parameters.
  --   For example
  --
  --   * F(x) is (FCall F [x] []).
  --   * F(x,y) would be (FCall F [x,y]).
  --   * F(x,n=y would be (FCall F [x] [(n,y)]).
  FCall    :: UID -> [Expr ?] -> [(UID, Expr ?)] -> Expr ?
\end{spec}

This is a certainly interesting one. The result's type should be the ``output''
of the function, and the function can) have mixed input types. It seems we'll
need to encode the function information differently.


\subsection{Case block}

\begin{spec}
  -- For multi-case expressions, each pair represents one case.
  Case     :: Completeness -> [(Expr Bool, Expr a)] -> Expr a
\end{spec}

This poses a bit of an issue if it's @Incomplete@.
It brings up the question of how we will define undefinedness of functions.
Should we perhaps throw an error in the programs in the event that the result of a @Case@ is @undefined@?

\subsection{Vector-related operations}

\begin{spec}
  -- Unary operation for vectors (holds whether a vector is normal or used for dimensions).
  UnaryOpVec    :: UFuncVec -> Expr (Vector a) -> Expr Double
  -- Binary operator for indexing two expressions.
  LABinaryOp    :: Indexable f => LABinOp -> Expr (f a) -> Expr Integer -> Expr a
  -- Binary operator for @Vector x Vector -> Vector@ operations (cross product).
  VVVBinaryOp   :: VVVBinOp -> Expr -> Expr -> Expr
  -- Binary operator for @Vector x Vector -> Number@ operations (dot product).
  VVNBinaryOp   :: (Indexable f, Num a) => VVNBinOp -> Expr (f a) -> Expr (f a) -> Expr ?
\end{spec}

Various 'attempts' above with my thoughts. I wonder if we should really be creating
different kinds of Vectors -- 2D Vectors, 3D Vectors, and N-ary Vectors (Lists).


\subsection{Matrices}

\begin{spec}
  -- Represents a matrix of expressions.
  Matrix   :: [[Expr a]] -> Expr [[Expr a]]
\end{spec}

It's a start, but I think that the shape could use work. We could probably make 
it @Expr (Matrix (Expr a))@ where @Matrix a = (Rows: Integer, Cols: Integer, Data: [[a]])@
just so that matrix size checking is easier later.

Right now, we don't generate code for matrix instantiation. I think we can create a basic
toolset of functions for each generated program to create "matrix objects" that they would
interface with. Alternatively, we could use existing libraries of course for matrix related
actions. 

\subsection{Operator}

\begin{spec}
  -- Operators are generalized arithmetic operators over a 'DomainDesc'
  --   of an 'Expr'.  Could be called BigOp.
  --   ex: Summation is represented via 'Add' over a discrete domain.
  Operator :: (Indexable f, Num a) => AssocArithOper -> 
              DomainDesc (Expr Integer) (Expr Integer) -> Expr (f a) -> Expr a
\end{spec}

I think it will be fine so as long as we strictly allow the discrete cases and straighten out our ``Indexable'' things.

\subsection{RealI}

\begin{spec}
  -- A different kind of 'IsIn'. A 'UID' is an element of an interval.
  RealI    :: Ord a => Expr a -> RealInterval (Expr a) (Expr a) -> Expr Bool
\end{spec}

No issue with this one. I also changed the @UID@ into an @Expr a@ since it allows for stricter typing and would also allow for expressions of the form @l < a*x+b < r@ too.

\end{document}
