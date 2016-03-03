type token =
  | INT of (int)
  | STRING of (string)
  | BOOL of (bool)
  | LIST of (list)
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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
    open Functions
    open Furyroad
# 28 "parser.ml"
let yytransl_const = [|
  261 (* PLUS *);
  262 (* MINUS *);
  263 (* TIMES *);
  264 (* DIV *);
  265 (* LESSTHAN *);
  266 (* EQUALTO *);
  267 (* GREATERTHAN *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* EOL *);
  271 (* FOR *);
  272 (* READ *);
  273 (* WRITE *);
  274 (* IF *);
  275 (* ELSE *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* STRING *);
  259 (* BOOL *);
  260 (* LIST *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\004\000\004\000\004\000\004\000\004\000\005\000\
\005\000\005\000\006\000\007\000\007\000\008\000\008\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\002\000\003\000\
\003\000\003\000\003\000\004\000\005\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\009\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\002\000\003\000\004\000\
\005\000\006\000\007\000\000\000\000\000\000\000\015\000\000\000\
\000\000\000\000\022\000\023\000\000\000\000\000\000\000\000\000\
\000\000\001\000\016\000\018\000\017\000\019\000\000\000\000\000\
\000\000\000\000\000\000\013\000\014\000\020\000\000\000\021\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000"

let yysindex = "\013\000\
\001\255\000\000\059\255\000\000\000\000\001\255\001\255\020\255\
\031\255\032\255\020\255\000\000\053\255\000\000\000\000\000\000\
\000\000\000\000\000\000\039\255\042\255\049\255\000\000\058\255\
\059\255\001\255\000\000\000\000\001\255\001\255\001\255\001\255\
\001\255\000\000\000\000\000\000\000\000\000\000\067\255\022\255\
\003\255\254\254\254\254\000\000\000\000\000\000\001\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\023\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\071\255\
\000\000\033\255\043\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\018\000\004\000\011\000\000\000\000\000"

let yytablesize = 79
let yytable = "\023\000\
\024\000\003\000\004\000\005\000\032\000\033\000\006\000\030\000\
\031\000\032\000\033\000\026\000\007\000\001\000\029\000\008\000\
\009\000\010\000\011\000\039\000\025\000\047\000\041\000\042\000\
\043\000\044\000\045\000\008\000\008\000\008\000\008\000\027\000\
\028\000\007\000\008\000\008\000\008\000\011\000\011\000\035\000\
\048\000\008\000\036\000\040\000\011\000\011\000\011\000\012\000\
\012\000\037\000\046\000\011\000\000\000\000\000\012\000\012\000\
\012\000\030\000\031\000\032\000\033\000\012\000\030\000\031\000\
\032\000\033\000\034\000\020\000\021\000\022\000\038\000\030\000\
\031\000\032\000\033\000\003\000\003\000\003\000\003\000"

let yycheck = "\006\000\
\007\000\001\001\002\001\003\001\007\001\008\001\006\001\005\001\
\006\001\007\001\008\001\008\000\012\001\001\000\011\000\015\001\
\016\001\017\001\018\001\026\000\001\001\019\001\029\000\030\000\
\031\000\032\000\033\000\005\001\006\001\007\001\008\001\001\001\
\001\001\012\001\012\001\013\001\014\001\005\001\006\001\001\001\
\047\000\019\001\001\001\026\000\012\001\013\001\014\001\005\001\
\006\001\001\001\040\000\019\001\255\255\255\255\012\001\013\001\
\014\001\005\001\006\001\007\001\008\001\019\001\005\001\006\001\
\007\001\008\001\014\001\009\001\010\001\011\001\013\001\005\001\
\006\001\007\001\008\001\005\001\006\001\007\001\008\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LESSTHAN\000\
  EQUALTO\000\
  GREATERTHAN\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  FOR\000\
  READ\000\
  WRITE\000\
  IF\000\
  ELSE\000\
  "

let yynames_block = "\
  INT\000\
  STRING\000\
  BOOL\000\
  LIST\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                            ( _1 )
# 151 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 29 "parser.mly"
                           ( _1 )
# 158 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'numericaloperator) in
    Obj.repr(
# 30 "parser.mly"
                           ( _1 )
# 165 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conditional) in
    Obj.repr(
# 31 "parser.mly"
                           ( _1 )
# 172 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bracketexpr) in
    Obj.repr(
# 32 "parser.mly"
                           ( _1 )
# 179 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'forloop) in
    Obj.repr(
# 33 "parser.mly"
                           ( _1 )
# 186 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func) in
    Obj.repr(
# 34 "parser.mly"
                           ( _1 )
# 193 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 38 "parser.mly"
                          ( INT _1 )
# 200 "parser.ml"
               : 'vartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                          ( STRING _1 )
# 207 "parser.ml"
               : 'vartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 40 "parser.mly"
                          ( BOOL _1 )
# 214 "parser.ml"
               : 'vartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
                            ( _1 + _3 )
# 222 "parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
                            ( _1 - _3 )
# 230 "parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                            ( _1 * _3 )
# 238 "parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                            ( _1 / _3 )
# 246 "parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                            ( - _2 )
# 253 "parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 52 "parser.mly"
                                ( (_1 < _3) )
# 261 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 53 "parser.mly"
                                ( (_1 > _3) )
# 269 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 54 "parser.mly"
                                ( (_1 = _3) )
# 277 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                           ( ( _2 ) )
# 284 "parser.ml"
               : 'bracketexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'conditional) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numericaloperator) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'bracketexpr) in
    Obj.repr(
# 61 "parser.mly"
                                                       ( while _2 do _3 ; _4 done)
# 293 "parser.ml"
               : 'forloop))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'conditional) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                                                       ( if _2 then _3 else _5 )
# 302 "parser.ml"
               : 'forloop))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 65 "parser.mly"
                               ( read _2 )
# 309 "parser.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 66 "parser.mly"
                               ( write _2 )
# 316 "parser.ml"
               : 'func))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : int)
