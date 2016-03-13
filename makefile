PP = str.cma
DEPEND += Lexer.mll Parser.mly
SOURCES = \
	Types.ml \
	EvaluationFunctions.ml \
  Functions.ml \
  Furyroad.ml \
  Parser.mli Parser.mly Lexer.mll \
  main.ml
RESULT = result.exe
OCAMLMAKEFILE = OCamlMakefile
LIBS = str
include $(OCAMLMAKEFILE)
