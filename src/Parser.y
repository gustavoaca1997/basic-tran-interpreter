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
%nonassoc '>' '<' '=' '>=' '<=' '/='
%left '+' '-'
%left '*' '/' '%'
%left or
%left and
%left not
%left '::'
%right '$'
%right '['
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
    | array '[' ExpArit ']' of Tipo { TipoArreglo $1 $3 $6  }


-------------------------------- EXPRESIONES ----------------------------------

Expresion : ExpArit               { ExpArit $1 }
        | ExpBool                 { ExpBool $1 }
        | ExpChar                 { ExpChar $1 }
        | ExpArray                { ExpArray $1 }

-- Expresion Arimetica
ExpArit : ExpArit '+' ExpArit     { Suma $1 $2 $3 }
        | ExpArit '-' ExpArit     { Resta $1 $2 $3 }
        | ExpArit '*' ExpArit     { Mult $1 $2 $3 }
        | ExpArit '/' ExpArit     { Div $1 $2 $3 }
        | ExpArit '%' ExpArit     { Mod $1 $2 $3 }
        | Menos ExpArit %prec NEG { MenosUnario $1 $2 }
        | '(' ExpArit ')'         { $2 }
        | id                      { IdArit $1 }
        | num                     { LitArit $1 }
        | '#' ExpChar             { Ascii $1 $2 }
        | IndexArray              { IndexArrayArit $1 }

IndexArray:
        ExpArray '[' ExpArit ']'      { IndexacionArray $1 $3 }

ExpRel : ExpArit '<'  ExpArit     { MenorQue $1 $2 $3 }
       | ExpArit '>'  ExpArit     { MayorQue $1 $2 $3 }
       | ExpArit '<=' ExpArit     { MenorIgualQue $1 $2 $3 }
       | ExpArit '>=' ExpArit     { MayorIgualQue $1 $2 $3 }
       | ExpArit '='  ExpArit     { Igual $1 $2 $3 }
       | ExpArit '/=' ExpArit     { Distinto $1 $2 $3 }

-- Expresiones Booleanas
ExpBool : ExpRel                            { Relacion $1 }
        | ExpBool and ExpBool               { OperadorBoolBin $1 $2 $3 }
        | ExpBool or ExpBool                { OperadorBoolBin $1 $2 $3 }
        | not ExpBool  %prec NEG            { OperadorBoolUn $1 $2 }
        | id                                { IdBool $1 }
        | true                              { LitBool $1 }
        | false                              { LitBool $1 }
        | '(' ExpBool ')'                   { $2 }
        | IndexArray                        { IndexArrayBool $1 }

-- Expresiones con caracteres
ExpChar : ExpChar '++'          { SiguienteChar $1 $2 }
        | ExpChar "--"          { AnteriorChar $1 $2 }
        | '(' ExpChar ')'               { $2 }
        | id                            { IdChar $1 }
        | caracter                      { LitChar $1 }
        | IndexArray                    { IndexArrayChar $1 }

-- Expresiones con arreglos
ExpArray : ExpArray '::' ExpArray       { ConcatenacionArray $1 $2 $3 }
        | '$' ExpArray                  { ShiftArray $1 $2 }
        | ExpArray '[' ExpArit ']'      { IndexacionArray $1 $3 }
        | id                            { IdArray $1 }
        | '(' ExpArray ')'              { $2 }


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
Condicional : If ExpBool '->' Bloque end                       { If $2 $4 }
            | If ExpBool '->' Bloque otherwise '->' Bloque end { IfOtherwise $2 $4 $7 }

-- Iteracion Determinada
IterDet : For id from ExpArit to ExpArit '->' Bloque end              { For $1 $2 $4 $6 $8 }
        | For id from ExpArit to ExpArit step ExpArit '->' Bloque end { ForStep $1 $2 $4 $6 $8 $10 }

-- Instrucciones I/O
IOInstr : Print Expresion           { Print $1 $2 }
        | Read  id                  { Read $1 $2 }

-- Asignacion
Asignacion : 
    id '<-' Expresion                           { Asignacion $1 $3 }

AsignacionIndexArray:
    ExpArray '[' ExpArit ']' '<-' Expresion           { AsignacionIndexArrayInstr (IndexacionArray $1 $3) $6}

-- Iteración Indeterminada
IteracionInd : While ExpBool '->' Bloque end            { WhileInstr $2 $4 }

-- Alcance
IncAlcance : With Variables begin Bloque end            { ConDeclaracion $1 $2 $4 }
           | Begin Bloque end                           { SinDeclaracion $1 $2 }

-- Instruccion Punto
PuntoInstr : id '.' ExpArit                                  { Punto $1 $2 $3  }

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
