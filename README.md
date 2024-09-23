[x] lexer
[ ] parser
[ ] evaluator

compile lexerREPL from root with `ghc lexer/lexerrepl.hs -main-is LexerREPL`
compile astREPL from /ast with `ghc parser.hs -main-is Parser -i../lexer/lexer.hs -i../lexer/token.hs -i../lexer/keywords.hs -i../ast/ast.hs`
