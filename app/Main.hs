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
    print tokens 
    print $ parser tokens
    -- (case (fst $ runState (semantic (parser tokens)) [empty]) of
    --     Left err -> putStrLn err
    --     Right _ -> putStrLn "La semantica esta correcta.")
