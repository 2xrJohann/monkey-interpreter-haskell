- [x] lexer
- [x] parser
- [x] evaluator
- [ ] vm
- [ ] compiler

## lexer repl
compile from root with `ghc lexer/lexerrepl.hs -main-is LexerREPL`

## parser repl
ghc parser.hs -main-is Parser -i../lexer/lexer.hs -i../lexer/token.hs -i../lexer/keywords.hs -i../ast/ast.hs

## evaluator repl
ghc parser.hs -main-is Evaluator -i ../evaluator/evaluator.hs -i../lexer/lexer.hs -i../lexer/token.hs -i../lexer/keywords.hs -i../ast/ast.hs
