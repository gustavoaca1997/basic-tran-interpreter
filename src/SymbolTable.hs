import Data.HashMap.Lazy
import Control.Monad.State
import Stack

-- Tabla de Símbolos por scope
type SymbolTable = HashMap String String

-- Pila de Tablas de Símbolos
type SymbolTableStack = Stack SymbolTable

-- Retorna y saca de la pila
popSTable :: State SymbolTableStack SymbolTable
popSTable = state $ pop

-- Insertar en la pila.
-- Se inserta una nueva tabla de símbolos tal que
-- tiene el nuevo scope y el anterior (sin símbolos del scope actual)
pushSTable :: SymbolTable -> State SymbolTableStack ()
pushSTable sTable =
    state (
        -- Recibe una pila, y retorna una pila con la nueva tabla
        \all@(x:xs) -> ((), (sTable `union` x):all)
    )