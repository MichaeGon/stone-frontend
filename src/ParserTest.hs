import Control.Monad
import System.Environment
import Text.Parsec

import StoneFrontend

main :: IO ()
main = getArgs >>= mapM_ (readFile >=> br . parseProgram)
    where
        br (Right x) = printAST x >> putStrLn ""
        br (Left x) = print x

printAST :: [Stmt] -> IO ()
printAST = mapM_ printStmt

printStmt :: Stmt -> IO ()
printStmt (If c b (Just e)) = putStr "(if " >> printExpr c  >> printStmt b >> putStr " else " >> printStmt e >> putStr ")"
printStmt (If c b _) = putStr "(if " >> printExpr c >> printStmt b >> putStrLn ")"
printStmt (While c b) = putStr "(while " >> printExpr c >> printStmt b >> putStr ")"
printStmt (Block ss) = putStr "(" >> printAST ss >> putStr ")"
printStmt (Single e) = putStr "(" >> printExpr e >> putStr ")"

printExpr :: Expr -> IO ()
printExpr (Un f) = printFactor f
printExpr (Bin l x r) = putStr "(" >> printExpr l >> putStr (" " ++ x ++ " ") >> printExpr r >> putStr ")"

printFactor :: Factor -> IO ()
printFactor (Neg p) = putStr "-" >> printPrimary p
printFactor (Pos p) = printPrimary p

printPrimary :: Primary -> IO ()
printPrimary (Paren e) = putStr "(" >> printExpr e >> putStr ")"
printPrimary (Num n) = putStr . show $ n
printPrimary (Id x) = putStr x
printPrimary (Str s) = putStr "\"" >> putStr s >> putStr "\""
