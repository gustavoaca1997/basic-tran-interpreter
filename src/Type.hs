module Type where

-- Tipos de datos presentes en el programa
data Type =
    Int Int
    | Char Char
    | Bool Bool
    | Array [Type]
    | None
    deriving Eq

instance Show Type where
    show (Int int) = show int

    show (Char char) = show char

    show (Bool bool) = show bool

    show (Array array) = show array

instance Num Type where
    Int a + Int b = Int $ a + b
    Int a - Int b = Int $ a - b
    Int a * Int b = Int $ a * b