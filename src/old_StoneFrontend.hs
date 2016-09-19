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

data Stmt = If Expr Stmt (Maybe Stmt) | While Expr Stmt | Block [Stmt] | Single Expr [Expr] | Def String [String] Stmt | Null
    deriving (Show)
data Expr = Un Factor | Bin Expr String Expr
    deriving (Show)
data Factor = Neg Primary | Pos Primary
    deriving (Show)
data Primary = Paren Expr | Num Integer | Id String | Str String | DefApp Primary [Expr]
    deriving (Show)

program :: Parser [Stmt]
program = whiteSpace' *> program'
    where
        program' = many (def <|> stmt)
        --program' = try (oneOf ";\n" *> program') <|> try ((:) <$> (def <|> stmt) <*> program') <|> return []
        {-}
        program' = try (oneOf ";\n" *> program') <|> try ((def <|> stmt) >>= rms) <|> return []
            where
                rms x = (x:) <$> program'
            -}
def :: Parser Stmt
def = reserved' "def" *> (Def <$> identifier' <*> paramList <*> blockstmt)
    where
        paramList = parens' . try $ commaSep' identifier'

whiteSpace' :: Parser ()
whiteSpace' = whiteSpace lexer

reserved' :: String -> Parser ()
reserved' = reserved lexer

parens' :: Parser a -> Parser a
parens' = parens lexer

commaSep' :: Parser a -> Parser [a]
commaSep' = commaSep lexer

identifier' :: Parser String
identifier' = identifier lexer

stmts :: Parser [Stmt]
stmts = many stmt
--stmts = try (oneOf ";\n" *> stmts) <|> try ((:) <$> stmt <*> stmts) <|> return []
{-}
stmts = try (oneOf ";\n" *> stmts) <|> try (stmt >>= rms) <|> return []
    where
        rms x = (x:) <$> stmts
-}

stmt :: Parser Stmt
stmt = choice
    [ nullstmt
    , ifstmt
    , whilestmt
    , blockstmt
    , single
    ]
    where
        nullstmt = try (Null <$ oneOf ";\n")
        ifstmt = reserved' "if" *> (If <$> expr <*> blockstmt <*> elseblock)
        elseblock = (reserved' "else" *> (Just <$> (ifstmt <|> blockstmt))) <|> return Nothing
        whilestmt = reserved' "while" *> (While <$> expr <*> blockstmt)
        single = Single <$> expr <*> postfix -- <* oneOf ";\n"
        {-postfix = try ([] <$ oneOf ";\n") <|> ((:) <$> expr <*> postfix')
            where
                postfix' = try ([] <$ oneOf ";\n") <|> (char ',' *> ((:) <$> expr <*> postfix'))
        --}
        postfix = commaSep' expr

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

primary :: Parser Primary
primary = primary' >>= checkArgs
    where
        primary' = choice
            [ Paren <$> try (parens' expr)
            , Num <$> try natural'
            , Id <$> try identifier'
            , Str <$> try stringLiteral'
            ]
        natural' = natural lexer
        stringLiteral' = stringLiteral lexer
        --checkArgs p = try (DefApp p <$> parens' postfix) <|> return p
        --postfix = try $ commaSep' expr
        --{-}
        checkArgs p = try (DefApp p <$> postfix) <|> return p
        postfix = parens' $ commaSep' expr
        --}
