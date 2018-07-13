module Semantic where

import SymbolTable
import Parsed
import ParsedTokens
import Data.HashMap.Lazy
import Control.Monad.State

-- Función que retorna error si hubo un error del parser, o
-- recorre el árbol para anlizarlo semanticamente
semantic :: (ToStr a) => Parsed a -> SymbolTableState
semantic (Failed err) = state(\s -> (Left err, s))
semantic (Ok parsed) = do
    traversal parsed