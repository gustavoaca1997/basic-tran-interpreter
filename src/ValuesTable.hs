module ValuesTable where
import qualified Data.HashMap.Lazy as H
import Control.Monad.State
import Type

-- Tabla de simbolos con valores
type ValuesTable = H.HashMap String Type
type ValuesTableState = StateT [ValuesTable] IO (Either String Type)

-- Función que hace merge de el scope nuevo con el actual
pushTable :: ValuesTable -> ValuesTableState
pushTable sTable' = do
    pila <- get
    (case pila of
        [] -> do
            put $ [H.empty]
        (x:xs) -> do
            put $ (sTable' `H.union` x):xs)
    return $ Right None
    -- state (
    --     \s -> ((Right None), (sTable' `H.union` (head s)):s)
    -- )

-- Funcion que pushea una tabla vacia
pushEmpty :: ValuesTableState
pushEmpty = do
    xs <- get
    put $ H.empty:xs
    return $ Right None
    -- state (
    --     \s -> (Right None, H.empty:s)
    -- )

-- Funcion que extrae de la tabla
popTable :: ValuesTableState
popTable = do
    pila <- get
    (case pila of
        [] -> do
            put $ [H.empty]
        (x:xs) -> do
            put xs)
    return $ Right None
    -- state (\s -> case s of
    --     [] -> (Right None, [H.empty])
    --     (x:xs) -> (Right None, tail s))