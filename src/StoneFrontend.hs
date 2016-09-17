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

data Stmt = If Expr Stmt (Maybe Stmt) | While Expr Stmt | Block [Stmt] | Single Expr --[Expr] | Def String [String] Stmt
    deriving (Show)
data Expr = Un Factor | Bin Expr String Expr
    deriving (Show)
data Factor = Neg Primary | Pos Primary
    deriving (Show)
data Primary = Paren Expr | Num Integer | Id String | Str String -- | DefApp Primary [Expr]
    deriving (Show)

program :: Parser [Stmt]
{-
program = try (oneOf ";\n" *> program) <|> try ((def <|> stmt) >>= rms) <|> return []
    where
        rms x = (x:) <$> program
-}
program = whiteSpace' *> stmts <* eof

{-}
def :: Parser Stmt
def = reserved' "def" *> (Def <$> identifier' <*> paramList <*> blockstmt)
    where
        paramList = parens' (commaSep1' identifier')
        commaSep1' = commaSep1 lexer
-}

whiteSpace' :: Parser ()
whiteSpace' = whiteSpace lexer

reserved' :: String -> Parser ()
reserved' = reserved lexer

parens' :: Parser a -> Parser a
parens' = parens lexer

identifier' :: Parser String
identifier' = identifier lexer

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
        --blockstmt = try (Block <$> braces' stmts)
        single = Single <$> expr
        --reserved' = reserved lexer
        --braces' = braces lexer

blockstmt :: Parser Stmt
blockstmt = try (Block <$> braces' stmts)
    where
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
        -- reserved' = reserved lexer

primary :: Parser Primary
primary = choice
    [ Paren <$> parens' expr
    , Num <$> try natural'
    , Id <$> try identifier'
    , Str <$> try stringLiteral'
    ]
    where
        --parens' = parens lexer
        natural' = natural lexer
        --identifier' = identifier lexer
        stringLiteral' = stringLiteral lexer
