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

    show (Char char) = [char]

    show (Bool bool) = show bool

    show (Array array) = show array

instance Num Type where
    Int a + Int b = Int $ a + b
    Int a - Int b = Int $ a - b
    Int a * Int b = Int $ a * b

-- Funcion para dividir
divType :: Type -> Type -> Type
divType (Int a) (Int b) = Int $ a `div` b

-- Funcion para el modulo
modType :: Type -> Type -> Type
modType (Int a) (Int b) = Int $ a `mod` b