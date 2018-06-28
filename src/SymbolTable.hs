module SymbolTable where
import qualified Data.HashMap.Lazy as H
import Control.Monad.State
import Lex


-- Tabla de Símbolos por scope
type SymbolTable = H.HashMap String String
type SymbolTableState = State [SymbolTable] (Either String SymbolTable)
-- Función que hace merge de el scope nuevo con el actual
pushSTable :: SymbolTable -> SymbolTableState
pushSTable sTable' =
    state (
        \s -> ((Right sTable'), (sTable' `H.union` (head s)):s)
    )
