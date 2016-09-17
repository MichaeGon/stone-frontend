module StoneFrontend
    ( Primary(..)
    , Factor(..)
    , Expr(..)
    , Stmt(..)
    , program
    , ops
    , Precedence(..)
    ) where

import StoneLexer

import Data.Map hiding (map)
import Data.Ord
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

data Precedence = Precedence {precedence :: Int, leftAssoc :: Bool }
    deriving (Show, Eq)

instance Ord Precedence where
    x `compare` y = comparing precedence x y `mappend` comparing leftAssoc y x

ops :: Map String Precedence
ops = fromList $ zip (reservedOpNames stoneDef) (map prec [(1, False), (2, True), (2, True), (2, True), (3, True), (3, True), (4, True), (4, True), (4, True)])
    where
        prec (x, y) = Precedence x y

data Primary = Paren Expr | Num Integer | Id String | Str String
    deriving (Show, Eq)
data Factor = Neg Primary | Prim Primary
    deriving (Show, Eq)
data Expr = Fact Factor | Bin Factor String Expr
    deriving (Show, Eq)
data Stmt = If Expr Stmt Stmt | While Expr Stmt | Block [Stmt] | None
    deriving (Show, Eq)

program :: Parser [Stmt]
program = stmts <* eof

stmt :: Parser Stmt
stmt =  ifstmt <|> whilestmt <|> block <|> return None
    where
        ifstmt = reserved' "if" *> (If <$> expr <*> block <*> elseblock)
        elseblock = reserved' "else" *> (ifstmt <|> block) <|> return None
        whilestmt = reserved' "while" *> (While <$> expr <*> block)
        block = Block <$> braces' stmts
        reserved' = reserved lexer
        braces' = braces lexer

stmts :: Parser [Stmt]
stmts = semiSep' stmt
    where
        semiSep' = semiSep lexer

expr :: Parser Expr
expr = binexpr <|> single
    where
        binexpr = Bin <$> factor <*> operator' <*> expr
        single = Fact <$> factor
        operator' = operator lexer

factor :: Parser Factor
factor = neg <|> prim
    where
        neg = reservedOp' "-" *> (Neg <$> primary)
        prim = Prim <$> primary
        reservedOp' = reservedOp lexer

primary :: Parser Primary
primary = parenexpr <|> numliteral <|> idnt <|> strliteral
    where
        parenexpr = Paren <$> parens' expr
        numliteral = Num <$> integer'
        idnt = Id <$> identifier'
        strliteral = Str <$> stringLiteral'
        parens' = parens lexer
        integer' = integer lexer
        identifier' = identifier lexer
        stringLiteral' = stringLiteral lexer
