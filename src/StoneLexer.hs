module StoneLexer
    ( lexer
    , stoneDef
    ) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token

import StoneAST (Env)

stoneDef :: LanguageDef Env
stoneDef = emptyDef
    { commentLine = "//"
    , identStart = letter
    , identLetter = alphaNum
    , reservedNames = ["if", "else", "while", "def", "fun", "class", "extends", "var", "Int", "String", "Any", "extern", "main"]
    , reservedOpNames = ["=", "==", ">", "<", "+", "-", "*", "/", "%", ".", ";", "[", "]", ":"]
    }

lexer :: TokenParser Env
lexer = makeTokenParser stoneDef
