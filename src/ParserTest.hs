import Control.Monad
import Data.List (foldl')
import System.Environment
import Text.Parsec

import StoneFrontend
import TypeCheck

main :: IO ()
main = getArgs >>= mapM_ (readFile >=> either print (mapM_ print) . parseProgramWithTypeCheck)
