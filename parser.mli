type token =
  | INT of (int)
  | STRING of (string)
  | BOOL of (bool)
  | LIST of (int list)
  | PLUS
  | MINUS
  | TIMES
  | DIV
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
