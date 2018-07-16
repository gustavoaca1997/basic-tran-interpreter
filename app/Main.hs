module Main where
import Lex
import Parser
import System.Environment
import Semantic
import Control.Monad.State
import SymbolTable
import Data.HashMap.Lazy
import Interprete
import System.IO

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    filecontents <- readFile $ head args
    let tokens = scanTokens filecontents
    -- print tokens 
    -- print $ parser tokens
    (
        let ast = (parser tokens) in
            case (fst $ runState (semantic ast) [empty]) of
            Left err -> putStrLn err
            Right _ -> do
                (ret, _) <- runStateT (interprete ast) [empty]
                (case ret of
                    Left err -> putStrLn err
                    Right _ -> return ()))
