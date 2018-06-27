module Main where
import Lex
import Parser
import System.Environment

main :: IO()
main = do
    args <- getArgs
    filecontents <- readFile $ head args
    let tokens = scanTokens filecontents
    print $ parser tokens
