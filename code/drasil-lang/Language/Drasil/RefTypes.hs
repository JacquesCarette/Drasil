-- | Contains the types associated to references
module Language.Drasil.RefTypes(RefAdd, LinkType(..)) where

type RefAdd = String

-- we do need to know a bit more about references, such as whether they are
-- an internal (document) link or a citation (and eventually, external).
-- basically, this amounts to a choice between
-- \ref (or \hyperref) and \cite in LaTeX.
-- in a sense, Cite2 is a very special kind of External reference.
data LinkType = Internal | Cite2 | External
