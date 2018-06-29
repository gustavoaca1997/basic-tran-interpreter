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

popSTable :: SymbolTableState
popSTable = state (\s -> (Right (head s), tail s))

-- Función que chequea si na variable está definida en el scope actual
inSTable :: String -> Int -> Int -> SymbolTableState
inSTable key l c =
    state (
        \s -> (
            let instable = H.member key (head s) in (
                case instable of
                    True -> (Right (head s), s)
                    False -> (Left (key ++ ": variable no declarada en la posicion " ++ show (l,c) ++ ": error semantico"), [H.empty] )
            )
        )
    )

-- Funcion que retorna el valor en la tabla de simbolos
checkType :: String -> String -> Int -> Int -> SymbolTableState
checkType key tipo l c =
    state (
        \s -> (
            let val = H.lookup key (head s) in (
                case val of
                    Nothing -> (Left (key ++ ": variable no declarada en la posicion " ++ show (l,c) ++ ": error semantico"), [H.empty] )
                    Just val' -> if val' == tipo then
                        (Right (head s), s)
                        else
                            (Left (key ++ ": variable de tipo " ++ val' ++ " no es de tipo " ++ tipo ++ " en la posicion " ++ show (l,c) ++ ": error semantico"), [H.empty] )    
            )
        )
    )