import Control.Monad
import System.Environment
import Text.Parsec

import StoneFrontend

main :: IO ()
main = getArgs >>= mapM_ (readFile >=> parseTest program)
