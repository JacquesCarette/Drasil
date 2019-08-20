-- | The parser that processes the configuration file specified as a command-line argument is defined here
module Language.Drasil.Code.Imperative.Parsers.ConfigParser (
    -- * Configuration for Code Generation
    Configuration(..),

    -- * Language Labels Used in the Configuration File
    cSharpLabel, cppLabel, goolLabel, javaLabel, objectiveCLabel, pythonLabel, luaLabel,

    -- * Processing the Configuration File
    readConfig
) where

import Control.Monad (liftM)
import Text.Parsec.Perm ((<|?>), (<$?>), (<$$>), (<||>), permute)
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, ParseError, parse, try, noneOf)
import Text.ParserCombinators.Parsec.Language (caseSensitive, reservedNames, reservedOpNames,
  opLetter, opStart, identLetter, identStart, nestedComments, commentLine, commentStart, 
  commentEnd, LanguageDef, emptyDef)

import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (Options(..))

-- | Data type that defines a configuration for generating code
-- Parameters: Language, optional parameters
data Configuration = Config {genLang :: String, exImp :: Maybe String, opts :: Options}
    deriving Show

cSharpLabel, cppLabel, goolLabel, javaLabel, objectiveCLabel, pythonLabel,
  luaLabel :: String
cSharpLabel = "C#"
cppLabel = "C++"
goolLabel = "GOOL"
javaLabel = "Java"
objectiveCLabel = "Objective-C"
pythonLabel = "Python"
luaLabel = "Lua"

--------------------------------
-- Config Language Definition --
--------------------------------

configDef :: LanguageDef st
configDef = emptyDef
            { commentStart      = "/*"
            , commentEnd        = "*/"
            , commentLine       = "//"
            , nestedComments    = True
            , identStart        = noneOf ", \t\n\r\f\v\xa0"
            , identLetter       = noneOf ", \t\n\r\f\v\xa0"
            , opStart           = opStart emptyDef
            , opLetter          = opLetter emptyDef
            , reservedOpNames   = ["="]
            , reservedNames     = ["Generation", "Language", "JavaListType", "CppListType", "ObjCStaticListType", "ExampleImplementation", "GOOLHsModule"]
            , caseSensitive     = False
            }

configLexer :: P.TokenParser st
configLexer = P.makeTokenParser configDef

identifier :: Parser String
identifier  = P.identifier configLexer

reserved, reservedOp :: String -> Parser ()
reserved    = P.reserved configLexer
reservedOp  = P.reservedOp configLexer

whiteSpace :: Parser ()
whiteSpace  = P.whiteSpace configLexer

-----------------------
-- Parsing Functions --
-----------------------

parseConfig :: Parser Configuration
parseConfig = do
    whiteSpace
    config <- permute $ Config <$$> try parseLang
                               <|?> (Nothing, Just `liftM` try (parseOption "ExampleImplementation"))
                               <||> try parseOptions
    -- parsing the options above always fails (puts Nothing in all Options fields, even if they have been specified), for some reason. Fix it by trying again:
    permute $ Config (genLang config) (exImp config) <$$> try parseOptions

parseLang :: Parser String
parseLang = do
    reserved "Generation"
    reserved "Language"
    reservedOp "="
    identifier

parseOptions :: Parser Options
parseOptions = permute $ Options <$?> (Nothing, Just `liftM` try (parseOption "JavaListType"))
                                 <|?> (Nothing, Just `liftM` try (parseOption "CppListType"))
                                 <|?> (Nothing, Just `liftM` try (parseOption "ObjCStaticListType"))
                                 <|?> (Nothing, Just `liftM` try (parseOption "GOOLHsModule"))

parseOption :: String -> Parser String
parseOption opt = do
    reserved opt
    reservedOp "="
    identifier

{-|
Parses the configuration file and returns a 'Configuration', or a 'ParseError' if the configuration file was not well-formed

'readConfig' takes the name of the file and the contents of the file
-}
readConfig :: String -> String -> Either ParseError Configuration
readConfig = parse parseConfig
