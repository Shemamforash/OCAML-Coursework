# 2 "Lexer.mll"
 
open Parser        (* The type token is defined in parser.mli *)
exception Eof

# 7 "Lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\058\000\116\000\192\000\250\000\052\001\110\001\245\255\
    \246\255\247\255\248\255\249\255\250\255\251\255\252\255\191\000\
    \254\255\255\255\168\001\226\001\028\002\086\002\144\002\202\002\
    \004\003\062\003\120\003\178\003\236\003\038\004\096\004";
  Lexing.lex_backtrk = 
   "\255\255\016\000\016\000\016\000\016\000\016\000\016\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\002\000\
    \255\255\255\255\016\000\016\000\011\000\016\000\016\000\016\000\
    \012\000\016\000\013\000\014\000\016\000\016\000\015\000";
  Lexing.lex_default = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\017\000\016\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \010\000\009\000\012\000\014\000\000\000\013\000\000\000\011\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\000\000\000\000\000\000\008\000\000\000\000\000\
    \000\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\001\000\001\000\001\000\002\000\004\000\001\000\
    \001\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\006\000\001\000\001\000\001\000\001\000\005\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \028\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \007\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\001\000\001\000\001\000\001\000\027\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\025\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\021\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
    \001\000\001\000\001\000\018\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\001\000\001\000\001\000\020\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\022\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\023\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\001\000\001\000\001\000\001\000\024\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \026\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\029\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
    \001\000\001\000\001\000\030\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\255\255\255\255\255\255\255\255\255\255\255\255\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\255\255\255\255\255\255\255\255\255\255\255\255\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\255\255\255\255\255\255\255\255\255\255\255\255\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\255\255\255\255\255\255\255\255\255\255\255\255\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec lexer lexbuf =
    __ocaml_lex_lexer_rec lexbuf 0
and __ocaml_lex_lexer_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 7 "Lexer.mll"
                     ( lexer lexbuf )
# 393 "Lexer.ml"

  | 1 ->
# 8 "Lexer.mll"
               ( EOL )
# 398 "Lexer.ml"

  | 2 ->
let
# 9 "Lexer.mll"
                    lxm
# 404 "Lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 9 "Lexer.mll"
                        ( INT(int_of_string lxm) )
# 408 "Lexer.ml"

  | 3 ->
# 10 "Lexer.mll"
               ( PLUS )
# 413 "Lexer.ml"

  | 4 ->
# 11 "Lexer.mll"
               ( MINUS )
# 418 "Lexer.ml"

  | 5 ->
# 12 "Lexer.mll"
               ( TIMES )
# 423 "Lexer.ml"

  | 6 ->
# 13 "Lexer.mll"
               ( DIV )
# 428 "Lexer.ml"

  | 7 ->
# 14 "Lexer.mll"
               ( LPAREN )
# 433 "Lexer.ml"

  | 8 ->
# 15 "Lexer.mll"
               ( RPAREN )
# 438 "Lexer.ml"

  | 9 ->
# 16 "Lexer.mll"
               ( EQUALS )
# 443 "Lexer.ml"

  | 10 ->
# 17 "Lexer.mll"
               ( raise Eof )
# 448 "Lexer.ml"

  | 11 ->
# 18 "Lexer.mll"
               ( READ )
# 453 "Lexer.ml"

  | 12 ->
# 19 "Lexer.mll"
               ( WRITE )
# 458 "Lexer.ml"

  | 13 ->
# 20 "Lexer.mll"
               ( FOR )
# 463 "Lexer.ml"

  | 14 ->
# 21 "Lexer.mll"
               ( IF )
# 468 "Lexer.ml"

  | 15 ->
# 22 "Lexer.mll"
               ( ELSE )
# 473 "Lexer.ml"

  | 16 ->
let
# 23 "Lexer.mll"
                           lxm
# 479 "Lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 23 "Lexer.mll"
                               ( VARIABLE )
# 483 "Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_lexer_rec lexbuf __ocaml_lex_state

;;

