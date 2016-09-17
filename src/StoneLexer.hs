module StoneLexer
    ( lexer
    , stoneDef
    , precedences
    , Precedence(..)
    ) where

import Data.Map
import Data.Ord
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token

data Precedence = Precedence {precedence :: Int, leftAssoc :: Bool }
    deriving (Show, Eq)

instance Ord Precedence where
    x `compare` y = comparing precedence x y `mappend` comparing leftAssoc y x

precedences :: Map String Precedence
precedences = fromList $ zip (reservedOpNames stoneDef) (fmap prec [(1, False), (2, True), (2, True), (2, True), (3, True), (3, True), (4, True), (4, True), (4, True)])
    where
        prec (x, y) = Precedence x y

stoneDef :: LanguageDef ()
stoneDef = emptyDef
    { commentLine = "//"
    , identStart = letter
    , identLetter = alphaNum
    , reservedNames = ["if", "else", "while", "def"]
    , reservedOpNames = ["=", "==", ">", "<", "+", "-", "*", "/", "%"{-, "&&", "||", ">=", "<="-}]
    }

lexer :: TokenParser ()
lexer = stone
    { whiteSpace = skipMany (simpleSpace' <|> oneLineComment <?> "")
    }
    where
        stone = makeTokenParser stoneDef
        simpleSpace' = skipMany1 $ satisfy (`elem` " \t\r\f\v")
        oneLineComment = try (string (commentLine stoneDef))
                    >> skipMany (satisfy (/= '\n'))
                    >> char '\n'
                    >> return ()
