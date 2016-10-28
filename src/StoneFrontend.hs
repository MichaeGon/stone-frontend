module StoneFrontend
    ( Primary(..)
    , Expr(..)
    , Stmt(..)
    , parseProgram
    , parseProgramWithTypeCheck
    --, program
    ) where

import StoneAST
import StoneLexer
import TypeCheck

import Control.Monad
import Text.Parsec hiding (Parser)
--import Text.Parsec.String
import Text.Parsec.Token

parseProgram :: String -> Either ParseError [Stmt]
parseProgram = runParser program singleton ""

parseProgramWithTypeCheck :: String -> Either ParseError [(Stmt, Type)]
parseProgramWithTypeCheck = runParser programWithTypeCheck singleton ""

program :: Parser [Stmt]
program = whiteSpace' *> many program' <* eof

programWithTypeCheck :: Parser [(Stmt, Type)]
programWithTypeCheck = whiteSpace' *> many (program' >>= typeCheck) <* eof

program' :: Parser Stmt
program' = sep *> stmt' <* sep
    where
        stmt' = choice
            [ defclass
            , def
            , stmt
            ]

defclass :: Parser Stmt
defclass = reserved' "class" *> (Class <$> identifier' <*> superclass <*> classbody)
    where
        superclass = optionMaybe $ reserved' "extends" *> identifier'
        classbody = braces' $ many stmt'
        stmt' = sep *> (def <|> variable) <* sep

def :: Parser Stmt
def = reserved' "def" *> (Def <$> identifier' <*> paramList <*> typetag <*> block)
    where
        paramList = parens' . commaSep' $ param
        param = (,) <$> identifier' <*> typetag

stmt :: Parser Stmt
stmt = choice
    [ variable
    , ifstmt
    , whilestmt
    , simple
    ]
    where
        ifstmt = reserved' "if" *> (If <$> expr <*> block <*> optionMaybe elsestmt)
        elsestmt = reserved' "else" *> (((:[]) <$> ifstmt) <|> block)
        whilestmt = reserved' "while" *> (While <$> expr <*> block)

variable :: Parser Stmt
variable = reserved' "var" *> (Var <$> identifier' <*> typetag <*> (reservedOp' "=" *> expr))

simple :: Parser Stmt
simple = Single <$> expr

block :: Parser [Stmt]
block = braces' $ many stmt'
    where
        stmt' = sep *> stmt <* sep

expr :: Parser Expr
expr = chainr1 l2s r1ops
    where
        l2s = chainl1 l3s l2ops
        l3s = chainl1 l4s l3ops
        l4s = chainl1 factor l4ops

        factor = (reservedOp' "-" *> (Neg <$> primary))
            <|> (Pos <$> primary)

        binop x = (\l r -> Bin l x r) <$ reservedOp' x
        ops = choice . fmap binop

        r1ops = ops ["="]
        l2ops = ops ["==", "<", ">"]
        l3ops = ops ["+", "-"]
        l4ops = ops ["*", "/", "%"]

primary :: Parser Primary
primary = closure <|> primary'
    where
        primary' = choice
            [ Num <$> try (natural lexer)
            , Id <$> identifier'
            , Str <$> try (stringLiteral lexer)
            , Paren <$> parens' expr
            , Array <$> brackets' (commaSep' expr)
            ] >>= check

        check p = option p $ postfix >>= check
            where
                postfix = (reservedOp' "." *> (Dot p <$> identifier'))
                    <|> (notFollowedBy (char '(') *> (Index p <$> brackets' expr))
                    <|> (DefApp p <$> parens' (commaSep' expr))

        closure = reserved' "fun" *> (Fun <$> params <*> typetag <*> block)
        params = parens' $ commaSep' param
        param = (,) <$> identifier' <*> typetag

typetag :: Parser Type
typetag = option Unknown $ reservedOp' ":" *> choice tags
    where
        tags =
            [ TInt <$ reserved' "Int"
            , TString <$ reserved' "String"
            , TAny <$ reserved' "Any"
            , TClassKey <$> identifier'
            ]

sep :: Parser ()
sep = void . many $ reservedOp' ";"

whiteSpace' :: Parser ()
whiteSpace' = whiteSpace lexer

reserved' :: String -> Parser ()
reserved' = reserved lexer

reservedOp' :: String -> Parser ()
reservedOp' = reservedOp lexer

identifier' :: Parser String
identifier' = identifier lexer

parens' :: Parser a -> Parser a
parens' = parens lexer

braces' :: Parser a -> Parser a
braces' = braces lexer

brackets' :: Parser a -> Parser a
brackets' = brackets lexer

commaSep' :: Parser a -> Parser [a]
commaSep' x = x `sepBy` (whiteSpace' *> char ',' <* whiteSpace')
