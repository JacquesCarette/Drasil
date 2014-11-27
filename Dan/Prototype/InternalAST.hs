--Most of this file is a placeholder and has already become deprecated.
--Cleanup will be performed soon.

module InternalAST where

-------------------------------------------------------------------------------
-- A Document can be defined by the Document Type
--  ex. SRS, Design Document, etc.
-- a style (TBD), list of definitions, and output filename.
-------------------------------------------------------------------------------
data Document = Document DType [Definition] FName
    deriving (Eq, Show)

data DType = SRS Style
            | Design Style
            | Testing Style
            | Code Style
            | LPM Style
    deriving (Eq, Show)
    
type Definition = (Name,Description)
            
type FName = String

type Style = String

type Name = String

--Description should be modified as it will contain more than just strings
--based on the field type. It can have a string, references to other chunks,
--or some combination of both.
type Description = String

something style doc = Document style doc "out"

-- main = do
    -- something (SRS "") [("Code","This is it")]