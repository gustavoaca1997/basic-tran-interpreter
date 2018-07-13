module Interprete where

import ValuesTable
import Type
import Data.HashMap.Lazy
import Control.Monad.State
import Parsed
import ParsedTokens

-- Funcion que retorna error si hubo un error del parser, o
-- recorre el arbol para interpretar las expresiones e instrucciones
interprete :: (ToStr a) => Parsed a -> ValuesTableState
interprete (Failed err) = return $ Left err
interprete (Ok parsed) =
    evaluar parsed