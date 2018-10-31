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
