\documentclass{article}

\usepackage{hyperref}

%include polycode.fmt

\begin{document}

Using the @Expr@ from the top of PR 2626's branch \href{https://github.com/JacquesCarette/Drasil/tree/658b6b84fc044d9ccb2fe86358385d81c713a75a}{658b6}, let's try to add typing.

\begin{code}
module Typing where
\end{code}


\tableofcontents

\subsection{Expr}
First, we parameterize @Expr@
\begin{spec}
data Expr a where
\end{spec}

\subsubsection{Primitives}
We add typing information for our ``primitives'', using @Double@ to represent @Real@s and assuming we want percentages to \textit{always} be @Real@s (potentially bad assumption, but easy fix).
\begin{spec}
  Dbl      :: Double -> Expr Double
  Int      :: Integer -> Expr Integer
  ExactDbl :: Integer -> Expr Double
  Str      :: String -> Expr String
  Perc     :: Integer -> Integer -> Expr Double
\end{spec}


\subsubsection{Associative Operators}

Ok, let's add typing for our associative operators
\begin{spec}
  -- Takes an associative arithmetic operator with a list of expressions.
  AssocA   :: Num a => AssocArithOper -> [Expr a] -> Expr a
  -- Takes an associative boolean operator with a list of expressions.
  AssocB   :: AssocBoolOper  -> [Expr Bool] -> Expr Bool
\end{spec}

Looks good.

\subsubsection{Derivatives}

@Deriv@ (Derivatives) next
\begin{spec}
  -- Derivative syntax is:
  --   Type ('Part'ial or 'Total') -> principal part of change -> with respect to
  --   For example: Deriv Part y x1 would be (dy/dx1).
  Deriv    :: DerivType -> Expr -> UID -> Expr  -- TODO?
\end{spec}

This one is very tough. With Issue \href{https://github.com/JacquesCarette/Drasil/issues/2628}{2628}, we'll be moving this into the @DisplayExpr@, so it won't be an issue for us, but assuming we wanted to add typing to it, it would be difficult. We have a few issues:

\begin{itemize}
  \item How do we deal with booleans? It appears we're severely limited here. The control flow would be left, but I don't know what we'd do with the ``derivative of an @Expr Bool@''.
  \item How do we deal with undefinedness?
\end{itemize}

There's lots more we can explore, but let's not dig too deep. Though, the thought here might be helpful elsewhere -- namely functions.


\subsubsection{Chunks}

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


I think this would be a good start, but I wonder if we would want to add typing to the actual quantities we have in Drasil. We'd then be able to pass the variables containing the quantities to C, instead of their UID. Who knows?


\subsubsection{Unary Operators}

\begin{spec}
  -- Unary operation for most functions (eg. sin, cos, log, etc.).
  UnaryOp       :: UFunc -> Expr Double -> Expr Double
  -- Unary operation for negation.
  UnaryOpB      :: UFuncB -> Expr Bool -> Expr Bool
  -- Unary operation for vectors (holds whether a vector is normal or used for dimensions).
  UnaryOpVec    :: UFuncVec -> Expr (Vector a) -> Expr Double
\end{spec}

Okay, starting to see some problems now. 
\begin{itemize}
  \item We might be able to split up our unary operators a bit more. Notably, the UnaryOps are always for doubles (that's not good!) -- we'll need to split it up a bit further to allow Absolute Value, Neg, etc for Integers as well as Doubles. Seems like a fairly simple fix. We could also add some extra features like truncating, rounding, casting, etc to allow for some extra things, but it's still fairly simple additions.
  \item The "Vector" is odd. What is it? Let's come back to it soon.
\end{itemize}

\end{document}