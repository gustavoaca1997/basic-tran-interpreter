module ParsedTokens where
import Lex
import SymbolTable
import Control.Monad.State
import qualified Data.HashMap.Lazy as H

-- Typeclass para poder imprimir el Arból Sintáctica Abstracto
class ToStr a where
    -- Funcion que convierte en string un token parseado
    -- donde el entero es el número de tabs
    toStr :: a -> Int -> String
    traversal :: a -> SymbolTableState


----------------------------------------------------------------------------
--------------------FUNCIONES PARA EL ANALIZADOR SEMANTICO -----------------
----------------------------------------------------------------------------

-- Función que recibe una lista de declaraciones de variables y dos estados: la tabla de símbolos global,
-- y la tabla de símbolos correspondiente solo al scope actual.
-- Retorna Left si ocurrió un error
varsToSTable :: [Variables] -> SymbolTable -> SymbolTableState
varsToSTable [] auxTable  = do
    pushSTable auxTable
varsToSTable ((Variables inits tipo):vars) auxTable = do
    ret <- initsToSTable inits tipo auxTable
    (case ret of
                Left err -> return ret
                -- Right auxTable' -> varsToSTable vars auxTable')
                Right _ -> state(\s -> runState (varsToSTable vars (head s)) s))

-- Función que recibe una lista de inicializacion de variables y dos estados: la tabla de símbolos global,
-- y la tabla de símbolos correspondiente solo al scope actual.
-- Retorna Left si ocurrió un error
initsToSTable :: [Inicializacion] -> Tipo -> SymbolTable -> SymbolTableState
-- Si ya no hay variables por analizar
initsToSTable [] _ auxTable = pushSTable auxTable

-- Si se inicializa la variable con un valor
initsToSTable ((Asignacion token expresion):xs) tipo auxTable
    | H.member (tkToStr token) auxTable = 
        state(\s -> (Left ("'" ++ (tkToStr token) ++ "' redeclarada en la posicion " ++ show (tkPos token) ++ ": error semantico"), [H.empty]))
    | otherwise = do
        ret <- initsToSTable xs tipo (H.insert (tkToStr token) (tipoToStr tipo) auxTable)
        (case ret of
            Left err -> return ret
            Right _ -> 
                -- Chequeamos que la expresion sea del tipo correcto
                -- (case expresion of
                --     ExpArit _ -> 
                --         if tipo' /= "int" then
                --             errorDeTipo
                --         else
                --             aciertoDeTipo
                --     ExpBool _ ->
                --         if tipo' /= "bool" then
                --             errorDeTipo
                --         else
                --             aciertoDeTipo
                --     ExpChar _ ->
                --         if tipo' /= "char" then
                --             errorDeTipo
                --         else
                --             aciertoDeTipo))
                aciertoDeTipo)

    where errorDeTipo = (return $ Left $ "Expresion de tipo distinto al tipo de '" ++ (tkToStr token) ++ "' en la posicion " ++ show(tkPos token) ++ ": error semantico") :: SymbolTableState
          aciertoDeTipo = state $ \s -> (Right $ tipo', s)
          tipo' = tipoToStr tipo

-- Si solo se declara la variable
initsToSTable ((Declaracion token):xs) tipo auxTable
    | H.member (tkToStr token) auxTable = 
        state(\s -> (Left ("'" ++ (tkToStr token) ++ "' redeclarada en la posicion " ++ show (tkPos token) ++ ": error semantico"), [H.empty]))
    | otherwise = initsToSTable xs tipo (H.insert (tkToStr token) (tipoToStr tipo) auxTable)

-- Función que recibe una lista de instrucciones y las recorre para analizar semanticamente
traverseList :: [Instruccion] ->  SymbolTableState
traverseList [] = pushSTable H.empty
traverseList (x:xs) = do
    ret <- traversal x
    (case ret of
                Left err -> state(\s -> (ret, s))
                Right _ -> do
                    popSTable
                    traverseList xs)


----------------------------------------------------------------------------
--------------------FUNCIONES PARA IMPRIMIR EL AST -------------------------
----------------------------------------------------------------------------
-- Funcion para imprimir listas de tokens
printLista :: (ToStr a) => Int -> [a] -> String
printLista tabs xs = concatMap (\x -> toStr x (tabs+2)) xs

-- Funcion para indentar
putTabs :: Int -> String -> String
putTabs tabs str = "\n" ++ (replicate (4*tabs) ' ') ++ str


----------------------------------------------------------------------------
-------------------------- TOKENS PARSEADOS --------------------------------
----------------------------------------------------------------------------

-- Variable inicial
data Programa
    =  Programa IncAlcanceInstr
    deriving Show

instance ToStr Programa where
    ----------------------------------------------------------------------------
    -- Para imprimir el AST
    toStr (Programa incalcance) tabs = toStr incalcance tabs

    ----------------------------------------------------------------------------
    -- Para analizar semanticamente el arbol
    traversal (Programa incalcance)= traversal incalcance

--------------------------------------------------------------------
-- Para la declariacion, inicializacion o asignacion de una variable
data Inicializacion
    = Asignacion TkObject Expresion -- id <- n, id <- 2 + x
    | Declaracion TkObject              -- id
    deriving Show
instance ToStr Inicializacion where
    --------------------------------------------------------------------
    -- Funcion para imprimir el arbol sintactico
    toStr (Asignacion obj exp) tabs = putTabs tabs "ASIGNACION" ++
        putTabs (tabs+2) "IDENTIFICADOR" ++ putTabs (tabs+4) (show obj) ++ toStr exp (tabs+2)
    toStr (Declaracion obj) tabs = putTabs tabs "DECLARACION" ++
        putTabs (tabs+2) "IDENTIFICADOR" ++ putTabs (tabs+4) (show obj)

    --------------------------------------------------------------------
    -- Funcion para recorrer y analizar semanticamente el arbol

    -- Asignacion
    traversal (Asignacion token expresion) =
        let (l,c) = tkPos (token) in (
            do
                -- Vemos si la variable esta declarada
                ret <- inSTable (tkToStr token) l c
                (case ret of
                    Left err -> return ret
                    Right tipo ->
                        -- Ahora chequeamos que el tipo de datos
                        -- coincida con la expresion
                        do
                            ret' <- traversal expresion
                            (case ret' of
                                Left err -> return ret'
                                Right tipo_exp -> 
                                    checkType (tkToStr token) tipo_exp l c)))

    -- Declaracion
    traversal (Declaracion tkobject) =
        let (l,c) = tkPos (tkobject) in (
            do
                inSTable (tkToStr tkobject) l c
        )
--------------------------------------------------------------------
-- Tipos de datos
data Tipo =
    TipoPrimitivo TkObject
    | TipoArreglo TkObject Expresion Tipo -- array [exparit] of tipo
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

-------------------------------------------------------------------------------
-- Variables
data Variables =
    Variables [Inicializacion] Tipo
    deriving Show

instance ToStr Variables where
    toStr (Variables xs tipo) tabs = putTabs tabs "DECLARACION/INICIALIZACION DE VARIABLES" ++
        printLista tabs xs ++
        toStr tipo (tabs+2)

-------------------------------------------------------------------------------
-------------------------------- EXPRESIONES ----------------------------------
-------------------------------------------------------------------------------
-- Funcion que analiza semanticamente operaciones aritmeticas binarias
analizarOpBinArit :: Expresion -> TkObject -> Expresion -> SymbolTableState
analizarOpBinArit exparit1 token exparit2 = do
    -- Analizamos semanticamente la primera expresion
    ret1 <- traversal exparit1
    (case ret1 of
        Left err -> state(\s -> (ret1, s))
        Right _ -> do
            -- Analizamos semanticamente la segunda expresion
            traversal exparit2)

-- Expresion
data Expresion =
    -------------------------------------------------------------------------------
    -- Identificador
    Ident TkObject
    -------------------------------------------------------------------------------
    -- Expresion Aritmética
    | Suma Expresion TkObject Expresion
    | Resta Expresion TkObject Expresion
    | Mult Expresion TkObject Expresion
    | Div Expresion TkObject Expresion
    | Mod Expresion TkObject Expresion
    | MenosUnario TkObject Expresion
    | LitArit TkObject
    | Ascii TkObject Expresion
    | IndexArrayArit Expresion

    -------------------------------------------------------------------------------
    -- Expresión Relacional
    | MenorQue Expresion TkObject Expresion
    | MayorQue Expresion TkObject Expresion
    | MenorIgualQue Expresion TkObject Expresion
    | MayorIgualQue Expresion TkObject Expresion
    | Igual Expresion TkObject Expresion
    | Distinto Expresion TkObject Expresion

    ------------------------------------------------------------------------------------
    -- Expresión Booleana/Lógica
    | Relacion Expresion -- 2 + n <= x
    | OperadorBoolBin Expresion TkObject Expresion  -- B and (x > 2)
    | OperadorBoolUn TkObject Expresion
    | LitBool TkObject  -- True, False
    | IndexArrayBool Expresion -- a[1]

    ------------------------------------------------------------------------------------
    -- Expresion de caracteres
    | SiguienteChar Expresion TkObject
    | AnteriorChar Expresion TkObject
    | LitChar TkObject
    | IndexArrayChar Expresion

    -- Expresion de arreglos
    | ConcatenacionArray Expresion TkObject Expresion
    | ShiftArray TkObject Expresion
    | IndexacionArray Expresion Expresion
    deriving Show
instance ToStr Expresion where
    -------------------------------------------------------------------------------
    -- Para imprimir arbol

    toStr (Ident token) tabs = putTabs tabs "IDENTIFICADOR" ++
        putTabs (tabs+2) (show token)

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

    -------------------------------------------------------------------------------
    -- Para imprimir el AST
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

    ------------------------------------------------------------------------------------
    -- Para imprimir el AST
    toStr (Relacion exprel) tabs = putTabs tabs "RELACION" ++ toStr exprel (tabs+2)

    toStr (OperadorBoolBin expbool1 obj expbool2) tabs = putTabs tabs "OPERADOR_BOOL_BIN" ++
        toStr expbool1 (tabs+2) ++ (putTabs (tabs+2) (show obj)) ++ toStr expbool2 (tabs+2)

    toStr (OperadorBoolUn obj expbool) tabs = putTabs tabs "OPERADOR_BOOL_UN" ++
        (putTabs (tabs+2) (show obj)) ++ toStr expbool (tabs+2)

    toStr (LitBool obj) tabs = (putTabs tabs "LITERAL BOOLEANO") ++ (putTabs (tabs+2) (show obj))

    toStr (IndexArrayBool indexarray) tabs = putTabs tabs "ARREGLO INDEXADO" ++
        toStr indexarray (tabs+2)

    ------------------------------------------------------------------------------------
    -- Para imprimir AST
    toStr (SiguienteChar expchar obj) tabs = putTabs tabs "SIG_CHAR" ++
        toStr expchar (tabs+2) ++
        putTabs (tabs+2) (show obj)

    toStr (AnteriorChar expchar obj) tabs = putTabs tabs "ANT_CHAR" ++
        toStr expchar (tabs+2) ++
        putTabs (tabs+2) (show obj)

    toStr (LitChar obj) tabs = (putTabs tabs "LITERAL DE CARACTER") ++ (putTabs (tabs+2) (show obj))

    toStr (IndexArrayChar indexarray) tabs = putTabs tabs "ARREGLO INDEXADO" ++
        toStr indexarray (tabs+2)

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

    -------------------------------------------------------------------------------
    -- Para analizar semanticamente

    -- Identificador
    traversal (Ident token) = 
        let (l,c) = tkPos token in (
            do
                inSTable (tkToStr token) l c
        )

    -- Suma
    traversal (Suma exparit1 token exparit2) = 
        analizarOpBinArit exparit1 token exparit2

    -- Resta
    traversal (Resta exparit1 token exparit2) =
        analizarOpBinArit exparit1 token exparit2

    -- Multiplicacion
    traversal (Mult exparit1 token exparit2) =
        analizarOpBinArit exparit1 token exparit2

    -- Division
    traversal (Div exparit1 token exparit2) =
        analizarOpBinArit exparit1 token exparit2        

    -- Modulo
    traversal (Mod exparit1 token exparit2) =
        analizarOpBinArit exparit1 token exparit2

    -- Menos Unario
    traversal (MenosUnario operador exparit) =
        traversal exparit

    -- Literal
    traversal (LitArit token) = state (\s -> (Right "int", s))

    -- Menor que
    traversal (MenorQue exparit1 token exparit2) =
        analizarOpBinArit exparit1 token exparit2

    -- Mayor que
    traversal (MayorQue exparit1 token exparit2) =
        analizarOpBinArit exparit1 token exparit2

    -- Menor o igual que
    traversal (MenorIgualQue exparit1 token exparit2) =
        analizarOpBinArit exparit1 token exparit2

    -- Mayor o igual que
    traversal (MayorIgualQue exparit1 token exparit2) =
        analizarOpBinArit exparit1 token exparit2

    -- Igual que
    traversal (Igual exparit1 token exparit2) =
        analizarOpBinArit exparit1 token exparit2

    -- Distinto que
    traversal (Distinto exparit1 token exparit2) =
        analizarOpBinArit exparit1 token exparit2

    -- Relacion
    traversal (Relacion exprel) = traversal exprel

    -- and, or
    traversal (OperadorBoolBin expbool1 token expbool2) = do
        ret <- traversal expbool1
        (case ret of
            Left err -> return ret
            Right _ ->
                traversal expbool2)

    -- not
    traversal (OperadorBoolUn token expbool) = do
        traversal expbool

    -- Literal
    traversal (LitBool token) = state (\s -> (Right "bool", s))

    -- Siguiente caracter
    traversal (SiguienteChar expchar obj) =
        traversal expchar

    -- Caracter anterior
    traversal (AnteriorChar expchar obj) =
        traversal expchar

    -- Literal
    traversal (LitChar token) = state (\s -> (Right "char", s))

--------------------------------- INSTRUCCIONES -------------------------------
-- Instruccion
data Instruccion =
    IfInstr IfInstr
    | ForInstr ForInstr
    | WhileInstr Expresion [Instruccion]
    | IOInstr IOInstr
    | AsignacionInstr Inicializacion
    | AsignacionIndexArrayInstr Expresion Expresion
    | IncAlcanceInstr IncAlcanceInstr
    | PuntoInstr PuntoInstr
    -- | Asignacion (ver arriba en inicializacion)
    | EmptyInstr
    deriving Show
--------------------------------------------------------------------
instance ToStr Instruccion where
    --------------------------------------------------------------------
    -- Funcion para imprimir el arbol
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
    ----------------------------------------------------------------
    -- Funcion para recorrer el arbol
    traversal (AsignacionInstr x) = traversal x

    traversal (IncAlcanceInstr instr) = traversal instr

    traversal (EmptyInstr) = state(\s -> (Right "", s))
--------------------------------------------------------------------

-- Instrucción de If
data IfInstr =
    If Expresion [Instruccion]
    | IfOtherwise Expresion [Instruccion] [Instruccion]
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
        Expresion     -- from
        Expresion     -- to
        [Instruccion] -- Instruccion
    | ForStep
        TkObject    -- posicion
        TkObject    -- id
        Expresion     -- from
        Expresion     -- to
        Expresion     -- step
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
----------------------------------------------------------------------------
-- Instrucción de Alcance
data IncAlcanceInstr =
    ConDeclaracion TkObject [Variables] [Instruccion]
    | SinDeclaracion TkObject [Instruccion]
    deriving Show

instance ToStr IncAlcanceInstr where
    ----------------------------------------------------------------------------
    -- Para imprimir arbol
    toStr (ConDeclaracion _ ys instruccion) tabs = putTabs tabs "INC_ALCANCE" ++
        printLista tabs ys ++
        printLista tabs instruccion

    toStr (SinDeclaracion _ instruccion) tabs = putTabs tabs "INC_ALCANCE" ++
        printLista tabs instruccion

    ----------------------------------------------------------------------------
    -- Para recorrer arbol y analizarlo semanticamente
    traversal (ConDeclaracion tkobject vars insts) = do
        ret <- varsToSTable vars H.empty
        (case ret of
                    Left err -> state(\s -> (ret, s))
                    Right _ -> do
                        traverseList insts)

    traversal (SinDeclaracion tkobject insts) = do
        traverseList insts
        

-- Instrucción de Punto
data PuntoInstr =
    Punto
        TkObject -- Solo id
        TkObject -- . position
        Expresion -- id o num
    deriving Show

instance ToStr PuntoInstr where
    toStr (Punto (TkObject (TkId id) _ _) _ expresion) tabs = putTabs tabs "INSTR_PUNTO" ++
        putTabs (tabs+2) "variable:" ++ id ++
        putTabs (tabs+2) "expresion:" ++ toStr expresion (tabs+2)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------

