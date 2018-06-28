module Semantic where

import SymbolTable
import Parsed
import ParsedTokens
import Data.HashMap.Lazy
import Control.Monad.State

-- Función que retorna error si hubo un error del parser, o
-- recorre el árbol
semantic :: (ToStr a) => Parsed a -> SymbolTableState
semantic (Failed err) = state(\s -> (Left err, s))
semantic (Ok parsed) = do
    traversal parsed

-- traversal :: (ToStr a) => Parsed a -> SymbolTable -> SymbolTableState
-- traversal tree@(Failed err) = \sTable -> state(\s -> (Left err, sTable))
-- traversal (Ok (ConDeclaracion tkobject vars insts)) = do
--     declaraciones <- varsToSTable vars
--     return declaraciones