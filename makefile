PP = str.cma
SOURCES = \
  Functions.ml \
  Furyroad.ml \
  parser.mli parser.mly lexer.mll \
  main.ml
RESULT = result.exe
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
