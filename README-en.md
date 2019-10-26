# Basic-Translation Interpreter
[Language specifications](https://drive.google.com/file/d/1Lsapac7c9lrTpRm5uTsGvMoeMd34Oorf/view?usp=sharing)
## Members:
* German Robayo (14-10924)
* Gustavo Castellanos (14-10192)

## Parts of the interpreter
### Lexer
#### What is it?

A lexer is a tool that allows us to analyze a sequence of characters and extract _tokens_ from it (lexicographic analysis) for its subsequent parsing.

#### Tool used

It was used as a programming language *__Haskell__* and as a generator tool for lexicographical analyzers *__Alex__*.
The use of Alex was fundamental, as we focused on only determining regular expressions to extract the tokens from the file.

#### Brief explanation

Alex provides several wrappers, but the one of our interest was `posn` because it provides the same functionality as the` basic` but adding the column and row of each detected token.

To implement the `show` of each token, a type of data` TkObject` was created that can be seen as an ordered pair whose first coordinate is a token and the second is its `AlexPosn`. Thus, it was avoided having to write a `show` for each token.

To filter errors, a token called `TkErr` was created. When we invoke `alexScanTokens` we have an array of `TkObject`'s. To see if there is an error, we only filter the TkObjects and if the resulting array is empty, there are no errors and each token is printed in the described format. Otherwise, only errors are printed.

#### Additional libraries

Any. Although certain functions could be imported, it was decided, to practice the language more, that we ourselves implement those functions.

## Parser

### What is it?

A perser is a computer program that analyzes a string of symbols (syntactically) according to the rules of a formal grammar.

#### Tool used

It was used as programming language *__Haskell__* and as a parser generating tool *__Happy__*.
The use of Happy focused us only on determining the grammar of the language.

#### Brief explanation

Through Happy, the grammar that defines the BasicTrans language was formed. This had to be modified during the development of the parser to eliminate conflicts _shift/reduce_ and _reduce/reduce_.

A `Parsed` data type was defined, which instance to the` Monad` typeclass, in order to know if an error occurred or not parsing.

The `ToStr` typeclass was defined, so that the Abstract Syntactic Tree could be printed on the console, which is instantiated by the types of data that define the analyzed tokens.

## Use of the interpreter

To compile the interpreter:
```bash
stack build
```
To run the interpreter:
```bash
stack exec BasicTran-Interpreter-exe <archive>
```
