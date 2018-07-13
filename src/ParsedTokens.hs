module ParsedTokens where
import Lex
import SymbolTable
import Control.Monad.State
import qualified Data.HashMap.Lazy as H
import Data.Either
import Data.List

-- Typeclass para poder imprimir el Arból Sintáctica Abstracto
class ToStr a where
    -- Funcion que convierte en string un token parseado
    -- donde el entero es el número de tabs
    toStr :: a -> Int -> String
    traversal :: a -> SymbolTableState


----------------------------------------------------------------------------
--------------------FUNCIONES PARA EL ANALIZADOR SEMANTICO -----------------
----------------------------------------------------------------------------
-- Funcion que analiza semanticamente operaciones aritmeticas binarias
analizarOpBin :: Expresion -> TkObject -> Expresion -> String -> SymbolTableState
analizarOpBin exparit1 token exparit2 tipo = do
    -- Analizamos semanticamente la primera expresion
    ret1 <- expIsOfType exparit1 tipo (tkPos token)
    (case ret1 of
        -- Vemos si ocurrio un error
        Left err -> return ret1
        Right exp_tipo1 -> do
            -- Analizamos semanticamente la segunda expresion
            ret2 <- expIsOfType exparit2 tipo (tkPos token)
            (case ret2 of
                -- Vemos si ocurrio un error
                Left err -> return ret2
                Right exp_tipo2 ->
                    if (exp_tipo1 /= exp_tipo2) then
                        return $ Left $ "Operacion invalida '" ++ (tkToStr token) ++ "' de " ++ exp_tipo1 ++ " con " ++ exp_tipo2 ++ " en la posicion " ++ show (tkPos token) ++ ": error semantico"
                        else
                            return $ ret1))

-- Funcion que analiza semanticamente la declaracion de un tipo arreglo
arrayIsRight :: Tipo -> SymbolTableState
arrayIsRight (TipoArreglo token expresion tipo_arr)= do
    ret <- expIsOfType expresion "int" (tkPos token)
    (case ret of
        Left err -> return ret
        Right _ ->
            case tipo_arr of
                -- chequeamos si es de varias dimensiones
                TipoArreglo _ _ _ ->
                    arrayIsRight tipo_arr
                _ ->
                    return ret)

-- Funcion que chequea si una expresion es del tipo correcto
expIsOfType :: Expresion -> String -> (Int, Int) -> SymbolTableState
expIsOfType expresion tipo (l, c) = do
    -- Analizamos expresion derecha
    ret1 <- traversal expresion
    -- Vemos si hubo errores
    case ret1 of
        Left err -> return ret1
        Right exp_tipo1 ->
            -- Chequeamos si es del tipo correcto
            if (length (words exp_tipo1) == 0) then
                return $ Left $ "exp_tipo1 50 vacio"
            else if (words exp_tipo1 !! 0) /= tipo then
                return $ Left $ "Expresion de tipo " ++ exp_tipo1 ++ " no es de tipo " ++ tipo ++ " en la posicion " ++ show (l, c) ++ ": error semantico"
                else
                    return $ Right $ exp_tipo1

-- Función que recibe una lista de declaraciones de variables y dos estados: la tabla de símbolos global,
-- y la tabla de símbolos correspondiente solo al scope actual.
-- Retorna Left si ocurrió un error
varsToSTable :: [Variables] -> SymbolTable -> SymbolTableState
varsToSTable [] auxTable  = 
    -- pushSTable auxTable
    state (
        \s@(x:y:xs) ->
            (Right "", (x `H.union` y):y:xs)
    )
varsToSTable ((Variables inits tipo):vars) auxTable = do
    ret <- initsToSTable inits tipo auxTable
    (case ret of
                Left err -> return ret
                -- Right auxTable' -> varsToSTable vars auxTable')
                Right _ -> state(\s@(x:xs) -> runState (varsToSTable vars x) s))

-- Función que recibe una lista de inicializacion de variables y dos estados: la tabla de símbolos global,
-- y la tabla de símbolos correspondiente solo al scope actual.
-- Retorna Left si ocurrió un error
initsToSTable :: [Inicializacion] -> Tipo -> SymbolTable -> SymbolTableState
-- Si ya no hay variables por analizar
initsToSTable [] _ auxTable = 
    -- pushSTable auxTable
    state (
        \s@(x:xs) ->
            (Right "", (auxTable `H.union` x):xs)
    )

-- Si se inicializa la variable con un valor
initsToSTable ((Asignacion token expresion):xs) tipo auxTable
    | H.member (tkToStr token) auxTable = 
        state(\s -> (Left ("'" ++ (tkToStr token) ++ "' redeclarada en la posicion " ++ show (tkPos token) ++ ": error semantico"), [H.empty]))
    | otherwise = do
        ret <- initsToSTable xs tipo (H.insert (tkToStr token) (tipoToStr tipo) auxTable)
        (case ret of
            Left err -> return ret
            Right _ -> let (l,c) = tkPos token in
                do
                    -- Chequeamos que la expresion sea del tipo correcto
                    exp_tipo <- traversal expresion
                    checkType (tkToStr token) (fromRight "" exp_tipo) l c)

    where errorDeTipo = (return $ Left $ "Expresion de tipo distinto al tipo de '" ++ (tkToStr token) ++ "' en la posicion " ++ show(tkPos token) ++ ": error semantico") :: SymbolTableState
          aciertoDeTipo = (state $ \s -> (Right $ tipo', s)) :: SymbolTableState
          tipo' = tipoToStr tipo

-- Si solo se declara la variable
initsToSTable ((Declaracion token):xs) tipo auxTable
    | H.member (tkToStr token) auxTable = 
        state(\s -> (Left ("'" ++ (tkToStr token) ++ "' redeclarada en la posicion " ++ show (tkPos token) ++ ": error semantico"), [H.empty]))
    | otherwise = do
        -- Chequeamos el tipo
        check_type <- (case tipo of
            TipoArreglo token expresion tipo_arr ->
                -- expIsOfType expresion "int" (tkPos token)
                arrayIsRight tipo
            _ -> return $ Right $ (tipoToStr tipo) )
        (case check_type of
            Left err -> return check_type
            Right _ ->
                initsToSTable xs tipo (H.insert (tkToStr token) (tipoToStr tipo) auxTable))

-- Función que recibe una lista de instrucciones y las recorre para analizar semanticamente
traverseList :: [Instruccion] ->  SymbolTableState
traverseList [] = state(\s -> (Right "", s))
traverseList (x:xs) = do
    ret <- traversal x
    (case ret of
                Left err -> state(\s -> (ret, s))
                Right _ -> do
                    case x of
                        IncAlcanceInstr instr -> do
                            popSTable
                            traverseList xs
                        ForInstr instr -> do
                            popSTable
                            traverseList xs
                        _ -> do
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
                        -- Chequeamos que el identificador no sea un iterador
                        if tipo == "iter" then
                            return $ Left $ "Asignacion a iterador no permitida, en la posicion " ++ show (tkPos token) ++ ": error semantico"
                            else
                                -- Ahora chequeamos que el tipo de datos
                                -- coincida con la expresion
                                do
                                    ret' <- traversal expresion
                                    (case ret' of
                                        Left err -> return ret'
                                        Right tipo_exp -> 
                                            do
                                                ret_id <- inSTable (tkToStr token) l c
                                                let tipo_id = fromRight "" ret_id in
                                                    if (tipo_id /= tipo_exp) then
                                                        return $ Left $ "Asignacion de " ++ tipo_exp ++ " a " ++ tipo_id ++ " en la posicion " ++ show(l,c) ++ ": error semantico"
                                                        else
                                                            return $ ret_id)))

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

    ------------------------------------------------------------------------------------
    -- Expresion de caracteres
    | SiguienteChar Expresion TkObject
    | AnteriorChar Expresion TkObject
    | LitChar TkObject

    -- Expresion de arreglos
    | ConcatenacionArray Expresion TkObject Expresion
    | ShiftArray TkObject Expresion
    | IndexacionArray Expresion TkObject Expresion
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

    toStr (MenosUnario obj exparit) tabs = (putTabs tabs "MENOS UNARIO") ++ (putTabs (tabs+2) (show obj)) ++ (toStr exparit (tabs+2))

    toStr (LitArit obj) tabs = (putTabs tabs "LITERAL ARITMETICO") ++ (putTabs (tabs+2) (show obj))

    -------------------------------------------------------------------------------
    -- Expresion relacional
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
    -- Expresion booleana
    toStr (Relacion exprel) tabs = putTabs tabs "RELACION" ++ toStr exprel (tabs+2)

    toStr (OperadorBoolBin expbool1 obj expbool2) tabs = putTabs tabs "OPERADOR_BOOL_BIN" ++
        toStr expbool1 (tabs+2) ++ (putTabs (tabs+2) (show obj)) ++ toStr expbool2 (tabs+2)

    toStr (OperadorBoolUn obj expbool) tabs = putTabs tabs "OPERADOR_BOOL_UN" ++
        (putTabs (tabs+2) (show obj)) ++ toStr expbool (tabs+2)

    toStr (LitBool obj) tabs = (putTabs tabs "LITERAL BOOLEANO") ++ (putTabs (tabs+2) (show obj))

    ------------------------------------------------------------------------------------
    -- Expresiones de caracteres
    toStr (SiguienteChar expchar obj) tabs = putTabs tabs "SIG_CHAR" ++
        toStr expchar (tabs+2) ++
        putTabs (tabs+2) (show obj)

    toStr (AnteriorChar expchar obj) tabs = putTabs tabs "ANT_CHAR" ++
        toStr expchar (tabs+2) ++
        putTabs (tabs+2) (show obj)

    toStr (LitChar obj) tabs = (putTabs tabs "LITERAL DE CARACTER") ++ (putTabs (tabs+2) (show obj))

    ------------------------------------------------------------------------------------
    -- Expresiones de arreglos
    toStr (ConcatenacionArray exparray1 obj exparray2) tabs =
        putTabs tabs "CONCAT_ARR" ++
        toStr exparray1 (tabs+2) ++
        putTabs (tabs+2) (show obj) ++
        toStr exparray2 (tabs+2)

    toStr (ShiftArray obj exparray) tabs =
        putTabs tabs "SHIF_ARR" ++
        putTabs (tabs+2) (show obj) ++
        toStr exparray (tabs+2)

    toStr (IndexacionArray exparray _ exparit) tabs =
        putTabs tabs "INDEX_ARR" ++
        putTabs (tabs+2) "arreglo:" ++ toStr exparray (tabs+2) ++
        putTabs (tabs+2) "indice:" ++ toStr exparit (tabs+2)

    -------------------------------------------------------------------------------
    -------------------------------------------------------------------------------
    -- Para analizar semanticamente

    -- Identificador
    traversal (Ident token) = 
        let (l,c) = tkPos token in (
            do
                inSTable (tkToStr token) l c
        )

    -------------------------------------------------------------------------------
    -- Expresion aritmeticas
    -- Suma
    traversal (Suma exparit1 token exparit2) = 
        analizarOpBin exparit1 token exparit2 "int"

    -- Resta
    traversal (Resta exparit1 token exparit2) =
        analizarOpBin exparit1 token exparit2 "int"

    -- Multiplicacion
    traversal (Mult exparit1 token exparit2) =
        analizarOpBin exparit1 token exparit2 "int"

    -- Division
    traversal (Div exparit1 token exparit2) =
        analizarOpBin exparit1 token exparit2 "int"      

    -- Modulo
    traversal (Mod exparit1 token exparit2) =
        analizarOpBin exparit1 token exparit2 "int"

    -- Menos Unario
    traversal (MenosUnario operador exparit) =
        traversal exparit

    -- Literal
    traversal (LitArit token) = state (\s -> (Right "int", s))

    -------------------------------------------------------------------------------
    -- Expresion relacional
    -- Menor que
    traversal (MenorQue exparit1 token exparit2) = do
        ret <- analizarOpBin exparit1 token exparit2 "int"
        (case ret of
            Left err -> return ret
            Right _ -> return $ Right "bool")

    -- Mayor que
    traversal (MayorQue exparit1 token exparit2) = do
        ret <- analizarOpBin exparit1 token exparit2 "int"
        (case ret of
            Left err -> return ret
            Right _ -> return $ Right "bool")

    -- Menor o igual que
    traversal (MenorIgualQue exparit1 token exparit2) = do
        ret <- analizarOpBin exparit1 token exparit2 "int"
        (case ret of
            Left err -> return ret
            Right _ -> return $ Right "bool")

    -- Mayor o igual que
    traversal (MayorIgualQue exparit1 token exparit2) = do
        ret <- analizarOpBin exparit1 token exparit2 "int"
        (case ret of
            Left err -> return ret
            Right _ -> return $ Right "bool")

    -- Igual que
    traversal (Igual exparit1 token exparit2) = do
        ret <- analizarOpBin exparit1 token exparit2 "int"
        (case ret of
            Left err -> return ret
            Right _ -> return $ Right "bool")

    -- Distinto que
    traversal (Distinto exparit1 token exparit2) = do
        ret <- analizarOpBin exparit1 token exparit2 "int"
        (case ret of
            Left err -> return ret
            Right _ -> return $ Right "bool")

    -------------------------------------------------------------------------------
    -- Expresion booleana
    -- Relacion
    traversal (Relacion exprel) = traversal exprel

    -- and, or
    traversal (OperadorBoolBin expbool1 token expbool2) = do
        analizarOpBin expbool1 token expbool2 "bool"

    -- not
    traversal (OperadorBoolUn token expbool) = do
        ret <- expIsOfType expbool "bool" (tkPos token)
        (case ret of
            Left err -> return ret
            Right _  -> return $ Right "bool")

    -- Literal
    traversal (LitBool token) = state (\s -> (Right "bool", s))

    -------------------------------------------------------------------------------
    -- Expresion de caracteres
    -- Siguiente caracter
    traversal (SiguienteChar expchar token) = do
        let t = "char"
        ret <- expIsOfType expchar t (tkPos token)
        (case ret of
            Left err -> return ret
            Right _  -> return $ Right t)

    -- Caracter anterior
    traversal (AnteriorChar expchar token) =
        traversal (SiguienteChar expchar token)

    -- Ascii, el tipo de expresion es numerico pero debe tener una
    -- expresion de tipo caracter
    traversal (Ascii token expresion) = do
        let t = "char"
        ret <- expIsOfType expresion "char" (tkPos token)
        (case ret of
            Left err -> return ret
            Right _ -> return $ Right "int")

    -- Literal
    traversal (LitChar token) = state (\s -> (Right "char", s))

    -------------------------------------------------------------------------------
    -- Expresion de arreglos
    -- Indexacion
    traversal (IndexacionArray expresion1 corchete expresion2) = do
        -- Analizamos los tipos
        ret1 <- expIsOfType expresion1 "array" (tkPos corchete)
        ret2 <- expIsOfType expresion2 "int" (tkPos corchete)
        -- Chequeamos por errores
        case ret1 of
            Left err -> return ret1
            Right exp_tipo1 ->
                case ret2 of
                    Left err -> return ret2
                    Right exp_tipo2 ->
                        -- Retornamos el tipo del elemento
                        return $ Right $ intercalate " " $ tail $ words $ exp_tipo1

    -- Concatenacion
    traversal (ConcatenacionArray expresion1 operador expresion2) = do
        analizarOpBin expresion1 operador expresion2 "array"

    -- Shift
    traversal (ShiftArray token expresion) = do
        expIsOfType expresion "array" (tkPos token)
--------------------------------- INSTRUCCIONES -------------------------------
-- Instruccion
data Instruccion =
    IfInstr IfInstr
    | ForInstr ForInstr
    | WhileInstr TkObject Expresion [Instruccion]
    | IOInstr IOInstr
    | AsignacionInstr Inicializacion
    | AsignacionIndexArrayInstr Expresion TkObject Expresion
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

    toStr (WhileInstr token expbool y) tabs = putTabs tabs "ITERACION INDETERMINADA" ++
        putTabs (tabs+2) "guardia:" ++ toStr expbool (tabs+2) ++
        putTabs (tabs+2) "bloque:" ++ printLista tabs y

    toStr (IOInstr x) tabs = putTabs tabs "" ++ toStr x tabs

    toStr (AsignacionInstr x) tabs = toStr x tabs

    toStr (AsignacionIndexArrayInstr indexarray token exp) tabs = putTabs tabs "ASIGNACION" ++
        putTabs (tabs+2) "objetivo:" ++ toStr indexarray (tabs+2) ++
        putTabs (tabs+2) "valor:" ++ toStr exp (tabs+2)

    toStr (IncAlcanceInstr x) tabs = toStr x tabs

    toStr (PuntoInstr x) tabs = putTabs tabs "" ++ toStr x tabs

    toStr (EmptyInstr) tabs = ""
    ----------------------------------------------------------------
    -- Funcion para recorrer el arbol
    traversal (AsignacionInstr x) = traversal x

    traversal (AsignacionIndexArrayInstr expresion1 token expresion2) = do
        ret1 <- traversal expresion1
        (case ret1 of
            Left err -> return ret1
            Right tipo1 -> do
                ret2 <- traversal expresion2
                case ret2 of
                    Left err -> return ret2
                    Right tipo2 ->
                        if tipo1 /= tipo2 then
                            return $ Left $ "Asignacion de " ++ tipo2 ++ " a " ++ tipo1 ++ " en la posicion " ++ show(tkPos token) ++ ": error semantico"
                            else
                                return $ Right $ tipo1)

    traversal (IncAlcanceInstr instr) = traversal instr

    traversal (IfInstr x) = traversal x

    traversal (ForInstr x) = traversal x

    traversal (WhileInstr token expbool insts) = do
        ret <- expIsOfType expbool "bool" (tkPos token)
        (case ret of
            Left err -> return ret
            Right tipo ->
                do
                    traverseList insts)

    traversal (IOInstr x) = traversal x

    traversal (PuntoInstr x) = traversal x

    traversal (EmptyInstr) = state(\s -> (Right "", s))
--------------------------------------------------------------------
--------------------------------------------------------------------
-- Instrucción de If
data IfInstr =
    If Expresion TkObject [Instruccion]
    | IfOtherwise Expresion TkObject [Instruccion] [Instruccion]
    deriving Show

instance ToStr IfInstr where
    --------------------------------------------------------------------
    -- Para imprimir el AST
    toStr (If expbool token instruccion) tabs = putTabs tabs "CONDICIONAL" ++
        putTabs (tabs+2) "guardia:" ++ toStr expbool (tabs+2) ++
        putTabs (tabs+2) "exito:" ++ printLista tabs instruccion

    toStr (IfOtherwise expbool token instruccion1 instruccion2) tabs = putTabs tabs "CONDICIONAL" ++
        putTabs (tabs+2) "guardia:" ++ toStr expbool (tabs+2) ++
        putTabs (tabs+2) "exito:" ++ printLista tabs instruccion1 ++
        putTabs (tabs+2) "fracaso:" ++ printLista tabs instruccion2

    --------------------------------------------------------------------
    -- Para analizar semanticamente el arbol
    -- if
    traversal (If expresion token insts) = do
        ret <- traversal expresion
        case ret of
            Left err -> return ret
            Right tipo ->
                if tipo /= "bool" then
                    return $ Left $ "Guardia de tipo " ++ tipo ++ " distinto a bool, en la posicion " ++ show (tkPos token) ++ ": error semantico"
                    else
                        traverseList insts

    -- otherwise
    traversal (IfOtherwise expresion token insts1 insts2) = 
        do
            ret <- traversal expresion
            (case ret of
                Left err -> return ret
                Right tipo -> 
                    if tipo /= "bool" then
                        return $ Left $ "Guardia de tipo " ++ tipo ++ " distinto a bool, en la posicion " ++ show (tkPos token) ++ ": error semantico" 
                        else do
                            ret1 <- traverseList insts1
                            case ret1 of
                                Left err -> return ret1
                                Right _ -> do
                                    traverseList insts2)

--------------------------------------------------------------------
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
    --------------------------------------------------------------------
    -- Para imprimir AST
    toStr (For _ _ from to bloque) tabs = putTabs tabs "ITERACION DETERMINADA" ++
        putTabs (tabs+2) "inicio:" ++ toStr from (tabs+2) ++
        putTabs (tabs+2) "final:" ++ toStr to (tabs+2) ++
        putTabs (tabs+2) "bloque:" ++ printLista tabs bloque
    toStr (ForStep _ _ from to step bloque) tabs = putTabs tabs "ITERACION DETERMINADA" ++
        putTabs (tabs+2) "inicio:" ++ toStr from (tabs+2) ++
        putTabs (tabs+2) "final:" ++ toStr to (tabs+2) ++
        putTabs (tabs+2) "step:" ++ toStr step (tabs+2) ++
        putTabs (tabs+2) "bloque:" ++ printLista tabs bloque

    --------------------------------------------------------------------
    -- Para analizar semanticamente

    -- For sin step
    traversal (For token ident exp_from exp_to insts) = do 
        ret <- analizarIterDet token ident exp_from exp_to Nothing
        (case ret of
            Left err -> return ret
            Right _ ->
                traverseList insts)

    -- For con step
    traversal (ForStep token ident exp_from exp_to exp_step insts) = do
        ret <- analizarIterDet token ident exp_from exp_to (Just exp_step)
        (case ret of
            Left err -> return ret
            Right _ ->
                traverseList insts)
        
-- Funcion para analizar las iteraciones determinadas
analizarIterDet :: TkObject -> TkObject -> Expresion -> Expresion -> Maybe Expresion -> SymbolTableState
analizarIterDet token ident exp_from exp_to step =
    do
        -- Se declara el iterador
        pushSTable $ H.singleton (tkToStr ident) "iter"
        -- Analizamos ambas expresiones
        ret_from <- traversal exp_from
        ret_to <- traversal exp_to

        ret_step <- (case step of
            Nothing -> return $ Right "int"
            Just exp_step -> do traversal exp_step )

        (case ret_from of
            Left err -> return ret_from
            Right tipo_from ->
                case ret_to of
                    Left err -> return ret_to
                    Right tipo_to ->
                        case ret_step of
                            Left err -> return ret_step
                            Right tipo_step ->
                                -- Chequeamos los tipos de las expresiones
                                if tipo_from /= "int" then
                                    return $ Left $ "Limite inferior no es una expresion aritmetica, en la posicion " ++ show (tkPos token) ++ ": error semantico"
                                    else if tipo_to /= "int" then
                                        return $ Left $ "Limite superior no es una expresion aritmetica, en la posicion " ++ show (tkPos token) ++ ": error semantico"
                                    else if tipo_step /= "int" then
                                        return $ Left $ "El paso de la iteracion determinada no es una expresion aritmetica, en la posicion " ++ show (tkPos token) ++ ": error semantico"
                                    else
                                        return $ Right "")

--------------------------------------------------------------------
-- Instrucción de I/O
data IOInstr =
    Print TkObject Expresion
    | Read TkObject TkObject
    deriving Show

instance ToStr IOInstr where
    --------------------------------------------------------------------
    -- Para imprimir ASt
    toStr (Print _ expresion) tabs = putTabs tabs "INSTRUCCION I/O" ++
        putTabs (tabs+2) "funcion: print" ++
        putTabs (tabs+2) "expresion:" ++
        toStr expresion (tabs+2)
    toStr (Read _ variable) tabs = putTabs tabs "INSTRUCCION I/O" ++
        putTabs (tabs+2) "funcion: read" ++
        putTabs (tabs+2) "variable:" ++ show variable

    --------------------------------------------------------------------
    -- Para el analizis semantico
    -- Print
    traversal (Print token expresion) = do
        traversal expresion

    -- Read
    traversal (Read token identificador) = 
        let (l,c) = tkPos identificador in
        (do
            ret <- inSTable (tkToStr identificador) l c
            (case ret of
                Left err -> return ret
                Right tipo ->
                    -- Chequear si la variable es int, bool, char
                    if tipo `elem` ["int", "bool", "char"] then
                        return ret
                        else
                            return $ Left $ "Instruccion read aplicada a un identificador que no es int, bool o char, en la posicion " ++ show (l,c) ++ ": error semantico"))

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
        ret' <- pushEmpty
        (case ret' of
            Left err -> return ret'
            Right _ -> do
                ret <- varsToSTable vars H.empty
                (case ret of
                            Left err -> state(\s -> (ret, s))
                            Right _ -> do
                                traverseList insts))

    traversal (SinDeclaracion tkobject insts) = do
        pushSTable H.empty
        traverseList insts
        
--------------------------------------------------------------------
-- Instrucción de Punto
data PuntoInstr =
    Punto
        TkObject -- Solo id
        TkObject -- . position
        Expresion -- id o num
    deriving Show

instance ToStr PuntoInstr where
    --------------------------------------------------------------------
    -- Para imprimir AST
    toStr (Punto (TkObject (TkId id) _ _) _ expresion) tabs = putTabs tabs "INSTR_PUNTO" ++
        putTabs (tabs+2) "variable:" ++ id ++
        putTabs (tabs+2) "expresion:" ++ toStr expresion (tabs+2)

    --------------------------------------------------------------------
    -- Para analizar semanticamente la instruccion
    traversal (Punto identificador token expresion) = 
        let (l,c) = tkPos identificador in
        do
            ret_id <- checkType (tkToStr identificador) "int" l c
            (case ret_id of
                Left err -> return $ ret_id
                Right tipo ->
                    let (l',c') = tkPos token in
                    do
                        expIsOfType expresion "int" (l', c'))

--------------------------------------------------------------------------------------------------------------------------------------------------------------------

