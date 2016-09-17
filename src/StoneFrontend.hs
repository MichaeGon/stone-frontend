module StoneFrontend
    ( Primary(..)
    , Factor(..)
    , Expr(..)
    , Stmt(..)
    , parseProgram
    , program
    ) where

import StoneLexer

import Data.Map
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

parseProgram :: String -> Either ParseError [Stmt]
parseProgram = parse program ""

data Stmt = If Expr Stmt (Maybe Stmt) | While Expr Stmt | Block [Stmt] | Single Expr
    deriving (Show)
data Expr = Un Factor | Bin Expr String Expr
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
expr = factor >>= checkOp
    where
        checkOp fct = try (operator' >>= checkFactor) <|> return (Un fct)
            where
                checkFactor op = factor >>= expr' [op] . (: [Un fct]) . Un

        expr' xxs@(x : xs) yys@(r : l : ys) = try (operator' >>= nextFactor) <|> build xxs yys
            where
                nextFactor op
                    | precedences ! x < precedences ! op = factor >>= expr' (op : xxs) . (: yys) . Un
                    | otherwise = factor >>= expr' (op : xs) . (\n -> n : Bin l x r : ys) . Un
        expr' _ _ = fail "internal parser error in expr'"

        build [] [y] = return y
        build (x : xs) (r : l : ys) = build xs (Bin l x r : ys)
        build _ _ = fail "internal parser error in build"
        
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
