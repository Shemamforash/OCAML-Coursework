type token =
  | INT of (int)
  | VARIABLE of (string)
  | BOOL of (bool)
  | LIST of (int list)
  | TYPE of (FuryRoad.furytype)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EQUALS
  | LESSTHAN
  | EQUALTO
  | GREATERTHAN
  | LPAREN
  | RPAREN
  | EOL
  | FOR
  | READ
  | WRITE
  | IF
  | ELSE

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Furyroad.furyterm
