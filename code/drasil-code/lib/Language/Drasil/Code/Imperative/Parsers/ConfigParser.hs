-- | The parser that processes the configuration file specified as a command-line argument is defined here
module Language.Drasil.Code.Imperative.Parsers.ConfigParser (
    -- * Configuration for Code Generation
    Configuration(..),

    -- * Language Labels Used in the Configuration File
    cSharpLabel, cppLabel, goolLabel, javaLabel, objectiveCLabel, pythonLabel, luaLabel,

    -- * Processing the Configuration File
    readConfig
) where

import Text.Parsec.Perm ((<|?>), (<$?>), (<$$>), (<||>), permute)
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, ParseError, parse, try, noneOf)
import Text.ParserCombinators.Parsec.Language (caseSensitive, reservedNames, reservedOpNames,
  opLetter, opStart, identLetter, identStart, nestedComments, commentLine, commentStart,
  commentEnd, LanguageDef, emptyDef)

import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (Options(..))

-- | Data type that defines a configuration for generating code.
-- Parameters: Language, optional parameters
data Configuration = Config {genLang :: String, exImp :: Maybe String, opts :: Options}
    deriving Show

-- | Labels for different programming languages.
cSharpLabel, cppLabel, goolLabel, javaLabel, objectiveCLabel, pythonLabel,
  luaLabel :: String
-- | "C#".
cSharpLabel = "C#"
-- | "C++".
cppLabel = "C++"
-- | "GOOL".
goolLabel = "GOOL"
-- | "Java".
javaLabel = "Java"
-- | "Objective-C".
objectiveCLabel = "Objective-C"
-- | "Python".
pythonLabel = "Python"
-- | "Lua".
luaLabel = "Lua"

--------------------------------
-- Config Language Definition --
--------------------------------

-- | Configuration language definition.
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

-- | Lexer using the configuration language definition.
configLexer :: P.TokenParser st
configLexer = P.makeTokenParser configDef

-- | Parses a legal identifier. Fails on reserved words.
identifier :: Parser String
identifier  = P.identifier configLexer

reserved, reservedOp :: String -> Parser ()
-- | Parses reserved words. Checks that the name is not a prefix of a valid identifier.
reserved    = P.reserved configLexer
-- | Parses reserved operators. Checks that the name is not a prefix of a valid operator.
reservedOp  = P.reservedOp configLexer

-- | Parses any whitespace.
whiteSpace :: Parser ()
whiteSpace  = P.whiteSpace configLexer

-----------------------
-- Parsing Functions --
-----------------------

-- | Parser for 'Configuration'.
parseConfig :: Parser Configuration
parseConfig = do
    whiteSpace
    config <- permute $ Config <$$> try parseLang
                               <|?> (Nothing, Just `fmap` try (parseOption "ExampleImplementation"))
                               <||> try parseOptions
    -- parsing the options above always fails (puts Nothing in all Options fields, even if they have been specified), for some reason. Fix it by trying again:
    permute $ Config (genLang config) (exImp config) <$$> try parseOptions

-- | Parser for the programming language.
parseLang :: Parser String
parseLang = do
    reserved "Generation"
    reserved "Language"
    reservedOp "="
    identifier

-- | Parser for options.
parseOptions :: Parser Options
parseOptions = permute $ Options <$?> (Nothing, Just `fmap` try (parseOption "JavaListType"))
                                 <|?> (Nothing, Just `fmap` try (parseOption "CppListType"))
                                 <|?> (Nothing, Just `fmap` try (parseOption "ObjCStaticListType"))
                                 <|?> (Nothing, Just `fmap` try (parseOption "GOOLHsModule"))

-- | Helper for parsing options.
parseOption :: String -> Parser String
parseOption opt = do
    reserved opt
    reservedOp "="
    identifier


-- | Parses the configuration file and returns a 'Configuration',
-- or a 'ParseError' if the configuration file was not well-formed.
-- Takes the name of the file and the contents of the file.
readConfig :: String -> String -> Either ParseError Configuration
readConfig = parse parseConfig
