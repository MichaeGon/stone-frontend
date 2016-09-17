module StoneLexer
    ( lexer
    , stoneDef
    ) where

import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token

stoneDef :: LanguageDef ()
stoneDef = emptyDef
    { commentLine = "//"
    , identStart = letter
    , identLetter = alphaNum
    , reservedNames = ["if", "else", "while"]
    , reservedOpNames = ["=", "==", ">", "<", "+", "-", "*", "/", "%"{-, "&&", "||", ">=", "<="-}]
    }

lexer :: TokenParser ()
lexer = makeTokenParser stoneDef
