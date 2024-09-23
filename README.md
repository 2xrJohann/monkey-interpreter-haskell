[x] lexer
[x] parser
[ ] evaluator
[ ] vm
[ ] compiler

## Lexer REPL
compile lexerREPL from root with `ghc lexer/lexerrepl.hs -main-is LexerREPL` <br><br>

## AST REPL
compile astREPL from /ast with `ghc parser.hs -main-is Parser -i../lexer/lexer.hs -i../lexer/token.hs -i../lexer/keywords.hs -i../ast/ast.hs`
