module Type where

-- Tipos de datos presentes en el programa
data Type =
    Int Int
    | Char Char
    | Bool Bool
    | Array [Type]
    | None
    | Undefined String

instance Show Type where
    show (Int int) = show int

    show (Char char) = [char]

    show (Bool True) = "true"
    show (Bool False) = "false"

    show (Array array) = show array

    show None = "none"

    show (Undefined str) = error "Elementos del arreglo sin inicializar"
    -- show (Undefined str) = "undefined"

instance Num Type where
    Int a + Int b = Int $ a + b
    Int a - Int b = Int $ a - b
    Int a * Int b = Int $ a * b

instance Eq Type where
    Int a == Int b = a == b
    Int a /= Int b = a /= b

instance Ord Type where
    Int a > Int b = a > b
    Int a >= Int b = a >= b
    Int a < Int b = a < b
    Int a < Int b = a <= b

-- Operador booleanos
andType :: Type -> Type -> Type
Bool a `andType` Bool b = Bool $ a && b

orType :: Type -> Type -> Type
Bool a `orType` Bool b = Bool $ a || b

notType :: Type -> Type
notType (Bool a) = Bool $ not a

-- Funcion para dividir
divType :: Type -> Type -> Type
divType (Int a) (Int b) = Int $ a `div` b

-- Funcion para el modulo
modType :: Type -> Type -> Type
modType (Int a) (Int b) = Int $ a `mod` b

-- Funcion que retorna la longitud de un arreglo
longitud :: Type -> Int
longitud (Array xs) = length xs