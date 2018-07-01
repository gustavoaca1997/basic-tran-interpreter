module SymbolTable where
import qualified Data.HashMap.Lazy as H
import Control.Monad.State
import Lex


-- Tabla de Símbolos por scope
type SymbolTable = H.HashMap String String
type SymbolTableState = State [SymbolTable] (Either String String)
-- Función que hace merge de el scope nuevo con el actual
pushSTable :: SymbolTable -> SymbolTableState
pushSTable sTable' =
    state (
        \s -> ((Right ""), (sTable' `H.union` (head s)):s)
    )

pushEmpty :: SymbolTableState
pushEmpty =
    state (
        \s -> (Right "", H.empty:s)
    )

popSTable :: SymbolTableState
popSTable = state (\s -> case s of
    [] -> (Right "", [H.empty])
    (x:xs) -> (Right "", tail s))

-- Función que chequea si la variable está definida en el scope actual
inSTable :: String -> Int -> Int -> SymbolTableState
inSTable key l c =

    state (
        \s -> (
            let val = H.lookup key (head s) in (
                case val of
                    Just tipo -> (Right tipo, s)
                    Nothing -> 
                        -- Chequeamos en el scope anterior
                        (if length s > 1 then
                            let val1 = H.lookup key ((s::[SymbolTable]) !! 1) in (
                                case val1 of
                                    Nothing -> 
                                        (Left ("'" ++ key ++ "': variable no declarada en la posicion " ++ show (l,c) ++ ": error semantico"), [H.empty])
                                    Just tipo' ->
                                        (Right tipo', s))
                            else
                                (Left ("'" ++ key ++ "': variable no declarada en la posicion " ++ show (l,c) ++ ": error semantico"), [H.empty]))
            )
        )
    )

    -- state (
    --     \s -> (
    --         let instable = H.lookup key (head s) in (
    --             case instable of
    --                 Just tipo -> (Right tipo, s)
    --                 Nothing -> (Left ("'" ++ key ++ "': variable no declarada en la posicion " ++ show (l,c) ++ ": error semantico"), [H.empty] )
    --         )
    --     )
    -- )

-- Funcion que revisa el valor en la tabla de simbolos
checkType :: String -> String -> Int -> Int -> SymbolTableState
checkType key tipo l c =
    state (
        \s -> (
            let val = H.lookup key (head s) in (
                case val of
                    Nothing -> 
                        -- Chequeamos en el scope anterior
                        (if length s > 1 then
                            let val1 = H.lookup key (s !! 1) in (
                                case val1 of
                                    Nothing -> 
                                        (Left ("'" ++ key ++ "': variable no declarada en la posicion " ++ show (l,c) ++ ": error semantico"), [H.empty])
                                    Just val' ->
                                        (chequear val', s))
                            else
                                (Left ("'" ++ key ++ "': variable no declarada en la posicion " ++ show (l,c) ++ ": error semantico"), [H.empty]))
                    Just val' -> 
                        (chequear val', s)
            )
        )
    )
    where
        chequear val' =
            if (length (words val') == 0) then
                Left $ "lista vacia 50 symboltable"
            else  if (words val' !!0) == tipo then
                Right tipo
                else
                    Left ("'" ++ key ++ "': variable de tipo " ++ show val' ++ " no es de tipo " ++ show tipo ++ " en la posicion " ++ show (l,c) ++ ": error semantico")