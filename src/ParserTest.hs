import Control.Monad
import Data.List (foldl')
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
printStmt (Single e xs) = putStr "(" >> printExpr e >> printArgs xs >> putStr ")"
    where
        printArgs [] = return ()
        printArgs ys = putStr "(" >> foldl' (\_ x -> printExpr x >> putStr ",") (return ()) ys >> putStr ")"
printStmt (Def n xs b) = putStr "(def " >> putStr n >> putStr " (" >> printArgs xs >> putStr ")" >> printStmt b >> putStr ")"
    where
        printArgs = foldl' (\_ x -> putStr x >> putStr ",") (return ())

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
printPrimary (DefApp p xs) = printPrimary p >> putStr "(" >> printArgs xs >> putStr ")"
    where
        printArgs = foldl' (\_ x -> printExpr x >> putStr ",") (return ())
