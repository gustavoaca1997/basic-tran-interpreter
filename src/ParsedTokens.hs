module ParsedTokens where
import Lex
import SymbolTable
import Control.Monad.State
import Data.HashMap.Lazy

-- Typeclass para poder imprimir el Arból Sintáctica Abstracto
class ToStr a where
    -- Funcion que convierte en string un token parseado
    -- donde el entero es el número de tabs
    toStr :: a -> Int -> String
    traversal :: a -> SymbolTable -> SymbolTableState

-- Función que a partir de una lista de declaraciones, crea una nueva
-- tabla de hash que representa el nuevo scope
varsToSTable :: [Variables] -> SymbolTable -> State SymbolTable (Either String SymbolTable)
varsToSTable [] sTable = state(\s -> (Right sTable, sTable))
varsToSTable ((Variables ((Asignacion token _):xs) tipo):vars) sTable =
    -- insert (tkToStr token) (tipoToStr tipo)
    -- varsToSTable vars
    varsToSTable vars $ insert (tkToStr token) (tipoToStr tipo) sTable
------------------------------------------------------------------------------------------------------------------------------------------------------------------

printLista tabs xs = concatMap (\x -> toStr x (tabs+2)) xs

-- Tipos de datos a retornar
-- Variable inicial
data Programa
    =  Programa IncAlcanceInstr
    deriving Show

instance ToStr Programa where
    toStr (Programa incalcance) tabs = toStr incalcance tabs
    traversal (Programa incalcance) sTable = traversal incalcance sTable

-- Para la declariacion o inicializacion de una variable
data Inicializacion
    = Asignacion TkObject Expresion -- id <- n, id <- 2 + x
    | Declaracion TkObject              -- id
    deriving Show

instance ToStr Inicializacion where
    toStr (Asignacion obj exp) tabs = putTabs tabs "ASIGNACION" ++
        putTabs (tabs+2) "IDENTIFICADOR\n" ++ putTabs (tabs+4) (show obj) ++ toStr exp (tabs+2)
    toStr (Declaracion obj) tabs = putTabs tabs "DECLARACION" ++
        putTabs (tabs+2) "IDENTIFICADOR\n" ++ putTabs (tabs+4) (show obj)

-- Tipos de datos
data Tipo =
    TipoPrimitivo TkObject
    | TipoArreglo TkObject ExpArit Tipo -- array [exparit] of tipo
    deriving Show

instance ToStr Tipo where
    toStr (TipoPrimitivo obj) tabs = putTabs tabs "TIPO PRIMITIVO" ++ putTabs (tabs+2) (show obj)
    toStr (TipoArreglo obj exparit tipo) tabs = putTabs tabs "TIPO ARREGLO" ++
        putTabs (tabs+2) "tamaño:" ++ toStr exparit (tabs+2) ++
        putTabs (tabs+2) "tipo de los elementos:" ++ toStr tipo (tabs+2)

-- Funcion que retorna el string del tipo
tipoToStr :: Tipo -> String
tipoToStr (TipoPrimitivo obj) = tkToStr obj
tipoToStr (TipoArreglo obj exparit tipo) = "array " ++ tipoToStr tipo

-- Variables
data Variables =
    Variables [Inicializacion] Tipo
    deriving Show

instance ToStr Variables where
    toStr (Variables xs tipo) tabs = putTabs tabs "DECLARACION/INICIALIZACION DE VARIABLES" ++
        printLista tabs xs ++
        toStr tipo (tabs+2)

-------------------------------- EXPRESIONES ----------------------------------
-- Expresion
data Expresion =
    ExpArit ExpArit
    | ExpBool ExpBool
    | ExpChar ExpChar
    | ExpArray ExpArray
    deriving Show

instance ToStr Expresion where
    toStr (ExpArit x) tabs = toStr x tabs
    toStr (ExpBool x) tabs = toStr x tabs
    toStr (ExpChar x) tabs = toStr x tabs
    toStr (ExpArray x) tabs = toStr x tabs

-- Expresion Aritmética
data ExpArit =
    Suma ExpArit TkObject ExpArit
    | Resta ExpArit TkObject ExpArit
    | Mult ExpArit TkObject ExpArit
    | Div ExpArit TkObject ExpArit
    | Mod ExpArit TkObject ExpArit
    | MenosUnario TkObject ExpArit
    | LitArit TkObject
    | IdArit  TkObject
    | Ascii TkObject ExpChar
    | IndexArrayArit ExpArray
    deriving Show

instance ToStr ExpArit where
    toStr (Suma exparit1 obj exparit2) tabs = (putTabs tabs "SUMA") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

    toStr (Resta exparit1 obj exparit2) tabs = (putTabs tabs "RESTA") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

    toStr (Mult exparit1 obj exparit2) tabs = (putTabs tabs "MULTIPLICACIÓN") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

    toStr (Div exparit1 obj exparit2) tabs = (putTabs tabs "DIVISIÓN") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

    toStr (Mod exparit1 obj exparit2) tabs = (putTabs tabs "MODULO") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

    toStr (Ascii obj expchar) tabs = putTabs tabs "ASCII" ++
        putTabs (tabs+2) (show obj) ++
        toStr expchar (tabs+2)

    toStr (IndexArrayArit indexarray) tabs = putTabs tabs "ARREGLO INDEXADO" ++
        toStr indexarray (tabs+2)

    toStr (MenosUnario obj exparit) tabs = (putTabs tabs "MENOS UNARIO") ++ (putTabs (tabs+2) (show obj)) ++ (toStr exparit (tabs+2))

    toStr (LitArit obj) tabs = (putTabs tabs "LITERAL ARITMETICO") ++ (putTabs (tabs+2) (show obj))

    toStr (IdArit obj) tabs = (putTabs tabs "IDENTIFICADOR") ++ (putTabs (tabs+2) (show obj))

-- Expresión Relacional
data ExpRel =
    MenorQue ExpArit TkObject ExpArit
    | MayorQue ExpArit TkObject ExpArit
    | MenorIgualQue ExpArit TkObject ExpArit
    | MayorIgualQue ExpArit TkObject ExpArit
    | Igual ExpArit TkObject ExpArit
    | Distinto ExpArit TkObject ExpArit
    deriving Show

instance ToStr ExpRel where
    toStr (MenorQue exparit1 obj exparit2) tabs = (putTabs tabs "MENOR_QUE") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

    toStr (MayorQue exparit1 obj exparit2) tabs = (putTabs tabs "MAYOR_QUE") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

    toStr (MenorIgualQue exparit1 obj exparit2) tabs = (putTabs tabs "MENOR_IG_QUE") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

    toStr (MayorIgualQue exparit1 obj exparit2) tabs = (putTabs tabs "MAYOR_IG_QUE") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

    toStr (Igual exparit1 obj exparit2) tabs = (putTabs tabs "IGUAL") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

    toStr (Distinto exparit1 obj exparit2) tabs = (putTabs tabs "DISTINTO") ++ (toStr exparit1 (tabs+2)) ++
        (putTabs (tabs+2) (show obj)) ++ (toStr exparit2 (tabs+2))

-- Expresión Booleana/Lógica
data ExpBool =
    Relacion ExpRel -- 2 + n <= x
    | OperadorBoolBin ExpBool TkObject ExpBool  -- B and (x > 2)
    | OperadorBoolUn  TkObject ExpBool
    | IdBool TkObject   -- if es_string
    | LitBool TkObject  -- True, False
    | IndexArrayBool ExpArray -- a[1]
    deriving Show

instance ToStr ExpBool where
    toStr (Relacion exprel) tabs = putTabs tabs "RELACION" ++ toStr exprel (tabs+2)

    toStr (OperadorBoolBin expbool1 obj expbool2) tabs = putTabs tabs "OPERADOR_BOOL_BIN" ++
        toStr expbool1 (tabs+2) ++ (putTabs (tabs+2) (show obj)) ++ toStr expbool2 (tabs+2)

    toStr (OperadorBoolUn obj expbool) tabs = putTabs tabs "OPERADOR_BOOL_UN" ++
        (putTabs (tabs+2) (show obj)) ++ toStr expbool (tabs+2)

    toStr (IdBool obj) tabs = putTabs tabs "IDENTIFICADOR" ++ (putTabs (tabs+2) (show obj))

    toStr (LitBool obj) tabs = (putTabs tabs "LITERAL BOOLEANO") ++ (putTabs (tabs+2) (show obj))

    toStr (IndexArrayBool indexarray) tabs = putTabs tabs "ARREGLO INDEXADO" ++
        toStr indexarray (tabs+2)

-- Expresion de caracteres
data ExpChar =
    SiguienteChar ExpChar TkObject
    | AnteriorChar ExpChar TkObject
    | IdChar TkObject
    | LitChar TkObject
    | IndexArrayChar ExpArray
    deriving Show

instance ToStr ExpChar where
    toStr (SiguienteChar expchar obj) tabs = putTabs tabs "SIG_CHAR" ++
        toStr expchar (tabs+2) ++
        putTabs (tabs+2) (show obj)

    toStr (AnteriorChar expchar obj) tabs = putTabs tabs "ANT_CHAR" ++
        toStr expchar (tabs+2) ++
        putTabs (tabs+2) (show obj)

    toStr (IdChar obj) tabs = putTabs tabs "IDENTIFICADOR" ++ (putTabs (tabs+2) (show obj))

    toStr (LitChar obj) tabs = (putTabs tabs "LITERAL DE CARACTER") ++ (putTabs (tabs+2) (show obj))

    toStr (IndexArrayChar indexarray) tabs = putTabs tabs "ARREGLO INDEXADO" ++
        toStr indexarray (tabs+2)

-- Expresion de arreglos
data ExpArray =
    ConcatenacionArray ExpArray TkObject ExpArray
    | ShiftArray TkObject ExpArray
    | IndexacionArray ExpArray ExpArit
    | IdArray TkObject
    deriving Show

instance ToStr ExpArray where
    toStr (ConcatenacionArray exparray1 obj exparray2) tabs =
        putTabs tabs "CONCAT_ARR" ++
        toStr exparray1 (tabs+2) ++
        putTabs (tabs+2) (show obj) ++
        toStr exparray2 (tabs+2)

    toStr (ShiftArray obj exparray) tabs =
        putTabs tabs "SHIF_ARR" ++
        putTabs (tabs+2) (show obj) ++
        toStr exparray (tabs+2)

    toStr (IndexacionArray exparray exparit) tabs =
        putTabs tabs "INDEX_ARR" ++
        putTabs (tabs+2) "arreglo:" ++ toStr exparray (tabs+2) ++
        putTabs (tabs+2) "indice:" ++ toStr exparit (tabs+2)

    toStr (IdArray obj) tabs =
        putTabs tabs "IDENTIFICADOR" ++ (putTabs (tabs+2) (show obj))

--------------------------------- INSTRUCCIONES -------------------------------
-- Instruccion
data Instruccion =
    IfInstr IfInstr
    | ForInstr ForInstr
    | WhileInstr ExpBool [Instruccion]
    | IOInstr IOInstr
    | AsignacionInstr Inicializacion
    | AsignacionIndexArrayInstr ExpArray Expresion
    | IncAlcanceInstr IncAlcanceInstr
    | PuntoInstr PuntoInstr
    -- | Asignacion (ver arriba en inicializacion)
    | EmptyInstr
    deriving Show

instance ToStr Instruccion where
    toStr (IfInstr x) tabs = toStr x tabs

    toStr (ForInstr x) tabs = putTabs tabs "" ++ toStr x tabs

    toStr (WhileInstr expbool y) tabs = putTabs tabs "ITERACION INDETERMINADA" ++
        putTabs (tabs+2) "guardia:" ++ toStr expbool (tabs+2) ++
        putTabs (tabs+2) "bloque:" ++ printLista tabs y

    toStr (IOInstr x) tabs = putTabs tabs "" ++ toStr x tabs

    toStr (AsignacionInstr x) tabs = toStr x tabs

    toStr (AsignacionIndexArrayInstr indexarray exp) tabs = putTabs tabs "ASIGNACION" ++
        putTabs (tabs+2) "objetivo:" ++ toStr indexarray (tabs+2) ++
        putTabs (tabs+2) "valor:" ++ toStr exp (tabs+2)

    toStr (IncAlcanceInstr x) tabs = toStr x tabs

    toStr (PuntoInstr x) tabs = putTabs tabs "" ++ toStr x tabs

    toStr (EmptyInstr) tabs = ""



-- Instrucción de If
data IfInstr =
    If ExpBool [Instruccion]
    | IfOtherwise ExpBool [Instruccion] [Instruccion]
    deriving Show

instance ToStr IfInstr where
    toStr (If expbool instruccion) tabs = putTabs tabs "CONDICIONAL" ++
        putTabs (tabs+2) "guardia:" ++ toStr expbool (tabs+2) ++
        putTabs (tabs+2) "exito:" ++ printLista tabs instruccion

    toStr (IfOtherwise expbool instruccion1 instruccion2) tabs = putTabs tabs "CONDICIONAL" ++
        putTabs (tabs+2) "guardia:" ++ toStr expbool (tabs+2) ++
        putTabs (tabs+2) "exito:" ++ printLista tabs instruccion1 ++
        putTabs (tabs+2) "fracaso:" ++ printLista tabs instruccion2

-- Instrucción de For
data ForInstr =
    For
        TkObject    -- posicion
        TkObject    -- id
        ExpArit     -- from
        ExpArit     -- to
        [Instruccion] -- Instruccion
    | ForStep
        TkObject    -- posicion
        TkObject    -- id
        ExpArit     -- from
        ExpArit     -- to
        ExpArit     -- step
        [Instruccion] -- Instruccion
    deriving Show

instance ToStr ForInstr where
    toStr (For _ _ from to bloque) tabs = putTabs tabs "ITERACION DETERMINADA" ++
        putTabs (tabs+2) "inicio:" ++ toStr from (tabs+2) ++
        putTabs (tabs+2) "final:" ++ toStr to (tabs+2) ++
        putTabs (tabs+2) "bloque:" ++ printLista tabs bloque
    toStr (ForStep _ _ from to step bloque) tabs = putTabs tabs "ITERACION DETERMINADA" ++
        putTabs (tabs+2) "inicio:" ++ toStr from (tabs+2) ++
        putTabs (tabs+2) "final:" ++ toStr to (tabs+2) ++
        putTabs (tabs+2) "step:" ++ toStr step (tabs+2) ++
        putTabs (tabs+2) "bloque:" ++ printLista tabs bloque

-- Instrucción de I/O
data IOInstr =
    Print TkObject Expresion
    | Read TkObject TkObject
    deriving Show

instance ToStr IOInstr where
    toStr (Print _ expresion) tabs = putTabs tabs "INSTRUCCION I/O" ++
        putTabs (tabs+2) "funcion: print" ++
        putTabs (tabs+2) "expresion:" ++
        toStr expresion (tabs+2)
    toStr (Read _ variable) tabs = putTabs tabs "INSTRUCCION I/O" ++
        putTabs (tabs+2) "funcion: read" ++
        putTabs (tabs+2) "variable:" ++ show variable

-- Instrucción de Alcance
data IncAlcanceInstr =
    ConDeclaracion TkObject [Variables] [Instruccion]
    | SinDeclaracion TkObject [Instruccion]
    deriving Show

instance ToStr IncAlcanceInstr where
    -- Para imprimir arbol
    toStr (ConDeclaracion _ ys instruccion) tabs = putTabs tabs "INC_ALCANCE" ++
        printLista tabs ys ++
        printLista tabs instruccion

    toStr (SinDeclaracion _ instruccion) tabs = putTabs tabs "INC_ALCANCE" ++
        printLista tabs instruccion

    -- Para recorrer arbol
    traversal (ConDeclaracion tkobject vars insts) = do
            declaraciones <- varsToSTable vars
            return declaraciones

-- Instrucción de Punto
data PuntoInstr =
    Punto
        TkObject -- Solo id
        TkObject -- . position
        ExpArit -- id o num
    deriving Show

instance ToStr PuntoInstr where
    toStr (Punto (TkObject (TkId id) _ _) _ expresion) tabs = putTabs tabs "INSTR_PUNTO" ++
        putTabs (tabs+2) "variable:" ++ id ++
        putTabs (tabs+2) "expresion:" ++ toStr expresion (tabs+2)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Funcion para indentar
putTabs :: Int -> String -> String
putTabs tabs str = "\n" ++ (replicate (4*tabs) ' ') ++ str
