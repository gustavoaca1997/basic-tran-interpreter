{
module Parser where
import Lex
import Parsed
import ParsedTokens
}

%name parser
%tokentype { TkObject }
%error { parseError }
%monad { Parsed }

%token

-- Reservadas
with        { TkObject TkWith _ _ }
end         { TkObject TkEnd _ _ }
var         { TkObject TkVar _ _ }
while       { TkObject TkWhile _ _ }
for         { TkObject TkFor _ _ }
from        { TkObject TkFrom _ _ }
to          { TkObject TkTo _ _ }
step        { TkObject TkStep _ _ }
if          { TkObject TkIf _ _ }
otherwise   { TkObject TkOtherwise _ _ }
of          { TkObject TkOf _ _ }
begin       { TkObject TkBegin _ _ }
print       { TkObject TkPrint _ _ }
read        { TkObject TkRead _ _ }

-- Tipos
int         { TkObject TkInt _ _ }
bool        { TkObject TkBool _ _ }
char        { TkObject TkChar _ _ }
array       { TkObject TkArray _ _ }

-- Literales
caracter    { TkObject (TkCaracter _) _ _ }
true        { TkObject TkTrue _ _ }
id          { TkObject (TkId _) _ _ }
false       { TkObject TkFalse _ _ }
num         { TkObject (TkNum _) _ _ }

-- Separadores
','         { TkObject TkComa _ _ }
'.'         { TkObject TkPunto _ _ }
';'         { TkObject TkPuntoYComa _ _ }
':'         { TkObject TkDosPuntos _ _ }
'('         { TkObject TkParAbre _ _ }
')'         { TkObject TkParCierra _ _ }
'['         { TkObject TkCorcheteAbre _ _ }
']'         { TkObject TkCorcheteCierra _ _ }
'->'        { TkObject TkHacer _ _ }
'<-'        { TkObject TkAsignacion _ _ }

-- Operadores
'+'         { TkObject TkSuma _ _ }
'-'         { TkObject TkResta _ _ }
'*'         { TkObject TkMult _ _ }
'/'         { TkObject TkDiv _ _ }
'%'         { TkObject TkMod _ _ }
and         { TkObject TkConjuncion _ _ }
or          { TkObject TkDisyuncion _ _ }
not         { TkObject TkNegacion _ _ }
'/='        { TkObject TkDesigual _ _ }
'<'         { TkObject TkMenor _ _ }
'<='        { TkObject TkMenorIgual _ _ }
'>'         { TkObject TkMayor _ _ }
'>='        { TkObject TkMayorIgual _ _ }
'='         { TkObject TkIgual _ _ }
'++'        { TkObject TkSiguienteCar _ _ }
"--"        { TkObject TkAnteriorCar _ _ }
'#'         { TkObject TkValorAscii _ _ }
'::'        { TkObject TkConcatenacion _ _ }
'$'         { TkObject TkShift _ _ }

-- Precedencias
%left not
%left or
%left and
%nonassoc '>' '<' '=' '>=' '<=' '/='
%left '+' '-'
%left '*' '/' '%'
%left '::'
%left '++'
%left "--"
%right '$'
%right '['
%right '('
%right '#'
%left NEG
-- Grammar
%%


-- Variable Inicial
S: IncAlcance                { Programa $1 }

With : with                                        { % return $1 }

-- Declaracion de las variables
Variables : Var Identificadores ':' Tipo               { [Variables (reverse $2) $4] }
    | Var Identificadores ':' Tipo Variables           { (Variables (reverse $2) $4) : $5 }

Var : var   { $1 }

-- Tipo de dato
Tipo : int  { TipoPrimitivo $1 }
    | char  { TipoPrimitivo $1 }
    | bool  { TipoPrimitivo $1 }
    | array '[' Expresion ']' of Tipo { TipoArreglo $1 $3 $6  }


-------------------------------- EXPRESIONES ----------------------------------

Expresion :
        -- Expresiones aritmeticas
          Expresion '+' Expresion       { Suma $1 $2 $3 }
          | Expresion '-' Expresion     { Resta $1 $2 $3 }
          | Expresion '*' Expresion     { Mult $1 $2 $3 }
          | Expresion '/' Expresion     { Div $1 $2 $3 }
          | Expresion '%' Expresion     { Mod $1 $2 $3 }
          | Menos Expresion %prec NEG   { MenosUnario $1 $2 }
          | num                         { LitArit $1 }
        -- Expresiones relacionales
          | Expresion '<' Expresion     { MenorQue $1 $2 $3 }
          | Expresion '>' Expresion     { MayorQue $1 $2 $3 }
          | Expresion '<=' Expresion    { MenorIgualQue $1 $2 $3 }
          | Expresion '>=' Expresion    { MayorIgualQue $1 $2 $3 }
          | Expresion '=' Expresion     { Igual $1 $2 $3 }
          | Expresion '/=' Expresion    { Distinto $1 $2 $3 }
        -- Expresiones de caracteres
          | Expresion '++'              { SiguienteChar $1 $2 }
          | Expresion "--"              { AnteriorChar $1 $2 }
          | caracter                    { LitChar $1 }
        -- Expresiones Booleanas
          | Expresion and Expresion     { And $1 $2 $3 }
          | Expresion or Expresion      { Or $1 $2 $3 }
          | not Expresion  %prec NEG    { OperadorBoolUn $1 $2 }
          | true                        { LitBool $1 }
          | false                       { LitBool $1 }

        -- Expresiones con arreglos
          | Expresion '::' Expresion    { ConcatenacionArray $1 $2 $3 }
          | '$' Expresion               { ShiftArray $1 $2 }
          | Expresion '[' Expresion ']' { IndexacionArray $1 $2 $3 }
          | '#' Expresion               { Ascii $1 $2 }
          | '(' Expresion ')'           { $2 }
          | id                          { Ident $1 }


Menos : '-'                       { $1 }
-- Lista de las variables a declarar e inicializar
Identificadores : Identificadores ',' Inicializacion        { $3:$1 }
                | Inicializacion                            { [$1] }

-- (identificador, literal o identificador)
Inicializacion : id                                       { Declaracion $1 }
                | Asignacion                              { $1 }

--------------------------------- INSTRUCCIONES -------------------------------
Bloque : {- labmda -}                                      { [EmptyInstr] }
       | Instruccion Bloque                                { $1:$2 }

Instruccion : Condicional                                 { IfInstr $1 }
            | IterDet                                     { ForInstr $1 }
            | IteracionInd                                { $1 }
            | Asignacion ';'                              { AsignacionInstr $1 }
            | AsignacionIndexArray ';'                    { $1 }
            | IOInstr ';'                                 { IOInstr $1 }
            | IncAlcance                                  { IncAlcanceInstr $1 }
            | PuntoInstr ';'                              { PuntoInstr $1 }

            -- Condicionales
Condicional : If Expresion '->' Bloque end                       { If $2 $3 $4 }
            | If Expresion '->' Bloque otherwise '->' Bloque end { IfOtherwise $2 $3 $4 $7 }

-- Iteracion Determinada
IterDet : For id from Expresion to Expresion '->' Bloque end              { For $1 $2 $4 $6 $8 }
        | For id from Expresion to Expresion step Expresion '->' Bloque end { ForStep $1 $2 $4 $6 $8 $10 }

-- Instrucciones I/O
IOInstr : Print Expresion           { Print $1 $2 }
        | Read  id                  { Read $1 $2 }

-- Asignacion
Asignacion :
    id '<-' Expresion                           { Asignacion $1 $3 }

AsignacionIndexArray:
    Expresion '[' Expresion ']' '<-' Expresion           { AsignacionIndexArrayInstr (IndexacionArray $1 $2 $3) $5 $6}

-- Iteración Indeterminada
IteracionInd : While Expresion '->' Bloque end            { WhileInstr $1 $2 $4 }

-- Alcance
IncAlcance : With Variables begin Bloque end            { ConDeclaracion $1 $2 $4 }
           | Begin Bloque end                           { SinDeclaracion $1 $2 }

-- Instruccion Punto
PuntoInstr : id '.' Expresion                                  { Punto $1 $2 $3  }

Begin : begin   { $1 }
While : while   { $1 }
Print : print   { $1 }
Read : read   { $1 }
If : if         { $1 }
For : for       { $1 }

{

parseError :: [TkObject] -> Parsed a
parseError = \line ->
        if length line > 0 then
                fail $ show (line!!0) ++ ": error de analisis sintactico"
        else
                fail "There was a problem parsing input"
}
