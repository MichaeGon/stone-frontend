module StoneLexer
    ( lexer
    , stoneDef
    ) where

import Text.Parsec
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
lexer = stone
    { whiteSpace = skipMany (simpleSpace' <|> oneLineComment <?> "")
    }
    where
        stone = makeTokenParser stoneDef
        simpleSpace' = skipMany1 (satisfy (`elem` " \t\r\f\v"))
        oneLineComment = try (string (commentLine stoneDef)) >> skipMany (satisfy (/= '\n')) >> return ()
