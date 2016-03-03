PP = str.cma
SOURCES = \
  Functions.ml \
  Furyroad.mli \
  parser.mli parser.mly lexer.mll \
  main.ml
RESULT = result.exe
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
