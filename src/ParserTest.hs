import Control.Monad
import Data.List (foldl')
import System.Environment
import Text.Parsec

import StoneFrontend
import TypeCheck

main :: IO ()
main = getArgs >>= mapM_ (\x -> putStrLn x >> readFile x >>= either print main' . parseProgram)
    where
        main' = mapM_ print . runTypeCheck
