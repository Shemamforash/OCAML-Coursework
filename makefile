PP = str.cma
DEPEND += Lexer.mll Parser.mly
SOURCES = \
  Functions.ml \
  Furyroad.ml \
  Parser.mli Parser.mly Lexer.mll \
  main.ml
RESULT = result.exe
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
