PP = str.cma
DEPEND += Lexer.mll Parser.mly
SOURCES = \
	Types.ml \
	Exceptions.ml \
	Printer.ml \
	EvaluationFunctions.ml \
  Functions.ml \
  Furyroad.ml \
  Parser.mli Parser.mly Lexer.mll \
  Main.ml
RESULT = mysplinterpreter.exe
OCAMLMAKEFILE = OCamlMakefile
LIBS = str
include $(OCAMLMAKEFILE)
