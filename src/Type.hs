module Type where

-- Tipos de datos presentes en el programa
data Type =
    Int Int
    | Char Char
    | Bool Bool
    | Array [Type]
    | None

instance Show Type where
    show (Int int) = show int

    show (Char char) = show char

    show (Bool bool) = show bool

    show (Array array) = show array