module Main where
import Lex
import Parser
import System.Environment
import Semantic
import Control.Monad.State
import SymbolTable
import Data.HashMap.Lazy

main :: IO()
main = do
    args <- getArgs
    filecontents <- readFile $ head args
    let tokens = scanTokens filecontents
    print $ fst $ runState (semantic (parser tokens)) empty
    print $ parser tokens
