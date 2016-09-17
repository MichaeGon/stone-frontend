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

import Data.Map
import Data.Ord
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

data Precedence = Precedence {precedence :: Int, leftAssoc :: Bool }
    deriving (Show, Eq)

instance Ord Precedence where
    x `compare` y = comparing precedence x y `mappend` comparing leftAssoc y x

ops :: Map String Precedence
ops = fromList $ zip (reservedOpNames stoneDef) (fmap prec [(1, False), (2, True), (2, True), (2, True), (3, True), (3, True), (4, True), (4, True), (4, True)])
    where
        prec (x, y) = Precedence x y

data Stmt = If Expr Stmt (Maybe Stmt) | While Expr Stmt | Block [Stmt] | Single Expr
    deriving (Show)
data Expr = Un Factor | Bin Factor String Expr
    deriving (Show)
data Factor = Neg Primary | Pos Primary
    deriving (Show)
data Primary = Paren Expr | Num Integer | Id String | Str String
    deriving (Show)

program :: Parser [Stmt]
program = stmts <* eof

stmts :: Parser [Stmt]
stmts = try (oneOf ";\n" *> stmts) <|> try (stmt >>= rms) <|> return []
    where
        rms x = (x:) <$> stmts

stmt :: Parser Stmt
stmt = choice
    [ ifstmt
    , whilestmt
    , blockstmt
    , single
    ]
    where
        ifstmt = reserved' "if" *> (If <$> expr <*> blockstmt <*> elseblock)
        elseblock = (reserved' "else" *> (Just <$> (ifstmt <|> blockstmt))) <|> return Nothing
        whilestmt = reserved' "while" *> (While <$> expr <*> blockstmt)
        blockstmt = try (Block <$> braces' stmts)
        single = Single <$> expr
        reserved' = reserved lexer
        braces' = braces lexer

expr :: Parser Expr
expr = factor >>= binexpr
    where
        binexpr fct = try (Bin fct <$> operator' <*> expr) <|> return (Un fct)
        operator' = choice . fmap (\x -> x <$ reservedOp lexer x) $ reservedOpNames stoneDef

factor :: Parser Factor
factor = (reserved' "-" *> (Neg <$> primary)) <|> (Pos <$> primary)
    where
        reserved' = reserved lexer

primary :: Parser Primary
primary = choice
    [ Paren <$> parens' expr
    , Num <$> try natural'
    , Id <$> try identifier'
    , Str <$> try stringLiteral'
    ]
    where
        parens' = parens lexer
        natural' = natural lexer
        identifier' = identifier lexer
        stringLiteral' = stringLiteral lexer
