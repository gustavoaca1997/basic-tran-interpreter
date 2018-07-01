# Interpretador de Basic-Translation
[Especificaciones del lenguaje](https://drive.google.com/file/d/1Lsapac7c9lrTpRm5uTsGvMoeMd34Oorf/view?usp=sharing)
## Integrantes:
* German Robayo (14-10924)
* Gustavo Castellanos (14-10192)

## Partes del interpretador
### Lexer 
#### ¿Que es?

Un lexer es una herramienta que nos permite analizar una secuencia de caracteres y extraer de ella _tokens_ (análisis lexicográfico) para su posterior parseo.

#### Herramienta usada

Se usó como lenguaje de programación *__Haskell__* y como herramienta generadora de analizadores lexicográficos *__Alex__*.
El uso de Alex fue fundamental, pues nos enfocó en solamente determinar expresiones regulares para extraer los tokens del archivo.

#### Breve explicación

Alex provee varios wrappers, pero el de nuestro interés fue `posn` pues provee la misma funcionalidad que el `basic` pero añadiendo la columna y fila de cada token detectado.

Para implementar el `show` de cada token, se creo un tipo de dato `TkObject` que puede verse como un par ordenado cuya primera coordenada es un token y segunda es su `AlexPosn`. Así, se evitó tener que escribir un `show` por cada token.

Para el filtrado de errores se creó un token llamado `TkErr`. Cuando invocamos a `alexScanTokens` tenemos un arreglo de `TkObject`'s. Para ver si hay error solamente filtramos los TkObjects y si el arreglo resultante es vacio pues no hay errores y se imprime cada token en el formato descrito. En caso contrario, solamente se imprimen errores.

#### Librerias adicionales

Ninguna. Aunque se pudieron importar ciertas funciones se decidió para practicar mas el lenguaje que nosotros mismos implementaramos dichas funciones.

## Parser

### ¿Qué es?
Un perser es un programa informático que analiza una cadena de símbolos (sintácticamente) de acuerdo a las reglas de una gramática formal.

#### Herramienta usada
Se usó como lenguaje de programación *__Haskell__* y como herramienta generadora de parsers *__Happy__*.
El uso de Happy  nos enfocó en solamente determinar la gramática del lenguaje.

#### Breve explicación
Mediante Happy se formó la gramática que define al lenguaje BasicTrans. Ésta se debió ir modificando durante el desarrollo del parser para eliminar conflictos _shift/reduce_ y _reduce/reduce_.

Se definió un tipo de dato `Parsed`, el cual instancia a la typeclass `Monad`, para poder saber si ocurrió un error o no parseando.

Se definió la typeclass `ToStr`, para poder imprimir en consola el Árbol Sintáctico Abstracto, la cual es instanciada por los tipos de datos que definen a los tokens analizados.

## Uso del interpretador

Para compilar el interpretador:
```bash
stack build
```
Para ejecutar el interpretador:
```bash
stack exec BasicTran-Interpreter-exe <archivo>
```