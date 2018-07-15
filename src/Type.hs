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

    show (Char char) = [char]

    show (Bool bool) = show bool

    show (Array array) = show array

instance Num Type where
    None + _ = None
    Int a + Int b = Int $ a + b

    None - _ = None
    Int a - Int b = Int $ a - b

    None * _ = None
    Int a * Int b = Int $ a * b


instance Eq Type where
    Int a == Int b = Bool $ a == b
    Int a /= Int b = Bool $ a /= b

instance Ord Type where
    Int a > Int b = Bool $ a > b
    Int a >= Int b = Bool $ a >= b
    Int a < Int b = Bool $ a < b
    Int a < Int b = Bool $ a <= b

-- Funcion para dividir
divType :: Type -> Type -> Type
divType (Int a) (Int b) = Int $ a `div` b

-- Funcion para el modulo
modType :: Type -> Type -> Type
modType (Int a) (Int b) = Int $ a `mod` b