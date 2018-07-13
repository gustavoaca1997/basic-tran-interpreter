module Main where
import Lex
import Parser
import System.Environment
import Semantic
import Control.Monad.State
import SymbolTable
import Data.HashMap.Lazy
import Interprete

main :: IO()
main = do
    args <- getArgs
    filecontents <- readFile $ head args
    let tokens = scanTokens filecontents
    -- print tokens 
    -- print $ parser tokens
    (
        let ast = (parser tokens) in
            case (fst $ runState (semantic ast) [empty]) of
            Left err -> putStrLn err
            Right _ -> 
                putStrLn "Semantica correcta")
