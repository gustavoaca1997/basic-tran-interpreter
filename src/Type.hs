module Type where

-- Tipos de datos presentes en el programa
data Type =
    Int Int
    | Char Char
    | Bool Bool
    | Array [Type]
    | None