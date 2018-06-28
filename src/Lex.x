{
module Lex where
import System.Environment
}

%wrapper "posn"

$letras = [a-zA-Z]
$numeros = 0-9
$alphanum = [a-zA-Z0-9\_]
$characters = [^\\\']

tokens :-
  -- spaces
  $white+       ;
  -- Palabras reservadas
  with              {\ap s -> TkObject TkWith s ap}
  end               {\ap s -> TkObject TkEnd s ap}
  var               {\ap s -> TkObject TkVar s ap}
  while             {\ap s -> TkObject TkWhile s ap}
  for               {\ap s -> TkObject TkFor s ap}
  from              {\ap s -> TkObject TkFrom s ap}
  to                {\ap s -> TkObject TkTo s ap}
  step              {\ap s -> TkObject TkStep s ap}
  begin             {\ap s -> TkObject TkBegin s ap}
  read              {\ap s -> TkObject TkRead s ap}
  print             {\ap s -> TkObject TkPrint s ap}
  from              {\ap s -> TkObject TkFrom s ap}
  to                {\ap s -> TkObject TkTo s ap}
  step              {\ap s -> TkObject TkStep s ap}
  of                {\ap s -> TkObject TkOf s ap}
  if                {\ap s -> TkObject TkIf s ap}
  otherwise                {\ap s -> TkObject TkOtherwise s ap}

  -- Tipos
  bool              {\ap s -> TkObject TkBool s ap}
  int               {\ap s -> TkObject TkInt s ap}
  char              {\ap s -> TkObject TkChar s ap}
  array             {\ap s -> TkObject TkArray s ap}

  -- numbers
  $numeros+            {\ap s -> TkObject (TkNum s) s ap}

  -- caracteres
  '($characters)'               {\ap s -> TkObject (TkCaracter s) s ap}
  '\\[\\nt\']'      {\ap s -> TkObject (TkCaracter s) s ap}

  -- Booleans
  true              {\ap s -> TkObject TkTrue s ap}
  false             {\ap s -> TkObject TkFalse s ap}

  -- separadores
  \,                 {\ap s -> TkObject TkComa s ap}
  \.                {\ap s -> TkObject TkPunto s ap}
  \;                {\ap s -> TkObject TkPuntoYComa s ap}
  ::                {\ap s -> TkObject TkConcatenacion s ap}
  :                 {\ap s -> TkObject TkDosPuntos s ap}
  \(                {\ap s -> TkObject TkParAbre s ap}
  \)                {\ap s -> TkObject TkParCierra s ap}
  \[                {\ap s -> TkObject TkCorcheteAbre s ap}
  \]                {\ap s -> TkObject TkCorcheteCierra s ap}
  \-\>               {\ap s -> TkObject TkHacer s ap}
  \<\-               {\ap s -> TkObject TkAsignacion s ap}

  -- Operadores
  \+\+              {\ap s -> TkObject TkSiguienteCar s ap}
  \+                {\ap s -> TkObject TkSuma s ap}
  \-\-              {\ap s -> TkObject TkAnteriorCar s ap}
  \-                {\ap s -> TkObject TkResta s ap}
  \*                {\ap s -> TkObject TkMult s ap}
  \/                 {\ap s -> TkObject TkDiv s ap}
  \%                 {\ap s -> TkObject TkMod s ap}
  \/\\               {\ap s -> TkObject TkConjuncion s ap}
  \\\/               {\ap s -> TkObject TkDisyuncion s ap}
  not               {\ap s -> TkObject TkNegacion s ap}
  \/=                {\ap s -> TkObject TkDesigual s ap}
  \<                {\ap s -> TkObject TkMenor s ap}
  \<=                {\ap s -> TkObject TkMenorIgual s ap}
  \>                {\ap s -> TkObject TkMayor s ap}
  \>=                {\ap s -> TkObject TkMayorIgual s ap}
  =                 {\ap s -> TkObject TkIgual s ap}
  \#                 {\ap s -> TkObject TkValorAscii s ap}
  \$                {\ap s -> TkObject TkShift s ap}

  -- id
  $letras($alphanum*)   {\ap s -> TkObject (TkId s) s ap}

  -- Cualquier cosa
  .                 {\ap s -> TkObject (TkErr (head s)) s ap}
{
-- Codigo

-- tipos de token
data Token =
  -- Palabras Reservadas
    TkWith
    | TkEnd
    | TkVar
    | TkWhile
    | TkFor
    | TkFrom
    | TkTo
    | TkStep
    | TkIf
    | TkOtherwise
    | TkOf
    | TkBegin
    | TkPrint
    | TkRead

    -- Tipos
    | TkInt
    | TkBool
    | TkChar
    | TkArray

    -- Literales
    | TkCaracter String
    | TkTrue
    | TkId String
    | TkFalse
    | TkNum String

    -- Separadores
    | TkComa
    | TkPunto
    | TkPuntoYComa
    | TkDosPuntos
    | TkParAbre
    | TkParCierra
    | TkCorcheteAbre
    | TkCorcheteCierra
    | TkHacer
    | TkAsignacion

    -- Operadores
    | TkSuma
    | TkResta
    | TkMult
    | TkDiv
    | TkMod
    | TkConjuncion
    | TkDisyuncion
    | TkNegacion
    | TkDesigual
    | TkMenor
    | TkMenorIgual
    | TkMayor
    | TkMayorIgual
    | TkIgual
    | TkSiguienteCar
    | TkAnteriorCar
    | TkValorAscii
    | TkConcatenacion
    | TkShift

    -- Error
    | TkErr Char
    deriving (Eq, Show)


-- par (Token, AlexPosn)
data TkObject = TkObject Token String AlexPosn deriving (Eq)
instance Show TkObject where
  -- Ejm: Error: Caracter inesperado '?' en la fila 3 2
  show (TkObject (TkErr tk) _ (AlexPn _ l c)) = "Error: Caracter inesperado " ++ ['"', tk, '"'] ++ " en la fila " ++ show l ++ ", columna " ++ show c
  -- Ejm: TkWhile 3 2
  show (TkObject tk s (AlexPn _ l c)) = s ++ " " ++ show l ++ " " ++ show c

-- Funcion que retorna el string que representa el token
tkToStr :: TkObject -> String
tkToStr (TkObject _ str _) = str


-- Funcion que retorna la posición del token
tkPos :: TkObject -> (Int, Int)
tkPos (TkObject _ _ (AlexPn _ l c)) = (l, c)
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

-- Acciones IO

group :: [TkObject] -> [[TkObject]]
group [] = []
--group [a] = [[a]]
group ent@((TkObject _ _ (AlexPn _ a _)):_) = equalA:(group rest)
  where equalA = takeWhile (\(TkObject _ _ (AlexPn _ a' _)) -> a' == a) ent
        rest = drop (length equalA) ent

formatln' :: [TkObject] -> String
formatln' xs = concatMap (\tk -> show tk ++ final tk) xs
        where final tk = if tk /= last xs then ", " else "\n"

formatln :: [[TkObject]] -> String
formatln = concatMap (formatln')

print_action :: [TkObject] -> IO()
print_action = putStr . formatln . group

print_errors :: [TkObject] -> IO()
print_errors = putStr . formatln . map (\a -> [a])

isError :: TkObject -> Bool
isError (TkObject (TkErr _) _ _) = True
isError _ = False

scanTokens :: String -> [TkObject]
scanTokens = alexScanTokens
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

}
