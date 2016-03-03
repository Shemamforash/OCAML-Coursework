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
\003\000\003\000\003\000\004\000\004\000\004\000\004\000\004\000\
\005\000\005\000\005\000\006\000\007\000\007\000\008\000\008\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\003\000\003\000\004\000\005\000\002\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\010\000\011\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\003\000\
\004\000\005\000\006\000\007\000\016\000\000\000\000\000\000\000\
\023\000\024\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\020\000\000\000\000\000\000\000\000\000\
\000\000\014\000\015\000\017\000\019\000\018\000\021\000\000\000\
\022\000"

let yydgoto = "\002\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000"

let yysindex = "\001\000\
\030\255\000\000\000\000\000\000\000\000\000\000\030\255\030\255\
\036\255\036\255\036\255\036\255\000\000\002\255\070\255\000\000\
\000\000\000\000\000\000\000\000\000\000\069\255\070\255\030\255\
\000\000\000\000\030\255\030\255\030\255\030\255\030\255\000\000\
\036\255\036\255\036\255\000\000\078\255\007\255\006\255\061\255\
\061\255\000\000\000\000\000\000\000\000\000\000\000\000\030\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\044\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\082\255\000\000\048\255\
\059\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\249\255\250\255\002\000\006\000\248\255\000\000\000\000"

let yytablesize = 90
let yytable = "\021\000\
\022\000\001\000\023\000\025\000\026\000\023\000\028\000\029\000\
\030\000\031\000\028\000\029\000\030\000\031\000\024\000\032\000\
\037\000\027\000\008\000\039\000\040\000\041\000\042\000\043\000\
\048\000\038\000\044\000\045\000\046\000\047\000\003\000\004\000\
\005\000\006\000\000\000\007\000\003\000\004\000\005\000\006\000\
\049\000\008\000\000\000\000\000\009\000\010\000\011\000\012\000\
\002\000\002\000\002\000\002\000\012\000\012\000\000\000\002\000\
\002\000\002\000\000\000\012\000\012\000\012\000\002\000\013\000\
\013\000\000\000\012\000\030\000\031\000\000\000\013\000\013\000\
\013\000\028\000\029\000\030\000\031\000\013\000\033\000\034\000\
\035\000\036\000\028\000\029\000\030\000\031\000\003\000\003\000\
\003\000\003\000"

let yycheck = "\007\000\
\008\000\001\000\009\000\010\000\011\000\012\000\005\001\006\001\
\007\001\008\001\005\001\006\001\007\001\008\001\009\000\014\001\
\024\000\012\000\012\001\027\000\028\000\029\000\030\000\031\000\
\019\001\024\000\033\000\034\000\035\000\038\000\001\001\002\001\
\003\001\004\001\255\255\006\001\001\001\002\001\003\001\004\001\
\048\000\012\001\255\255\255\255\015\001\016\001\017\001\018\001\
\005\001\006\001\007\001\008\001\005\001\006\001\255\255\012\001\
\013\001\014\001\255\255\012\001\013\001\014\001\019\001\005\001\
\006\001\255\255\019\001\007\001\008\001\255\255\012\001\013\001\
\014\001\005\001\006\001\007\001\008\001\019\001\009\001\010\001\
\011\001\013\001\005\001\006\001\007\001\008\001\005\001\006\001\
\007\001\008\001"

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
# 160 "parser.ml"
               : Furyroad.furyterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 29 "parser.mly"
                           ( _1 )
# 167 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'numericaloperator) in
    Obj.repr(
# 30 "parser.mly"
                           ( _1 )
# 174 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conditional) in
    Obj.repr(
# 31 "parser.mly"
                           ( _1 )
# 181 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bracketexpr) in
    Obj.repr(
# 32 "parser.mly"
                           ( _1 )
# 188 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'forloop) in
    Obj.repr(
# 33 "parser.mly"
                           ( _1 )
# 195 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func) in
    Obj.repr(
# 34 "parser.mly"
                           ( _1 )
# 202 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 38 "parser.mly"
                          ( FuryInt _1 )
# 209 "parser.ml"
               : 'vartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                          ( FuryString _1 )
# 216 "parser.ml"
               : 'vartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 40 "parser.mly"
                          ( FuryBool _1 )
# 223 "parser.ml"
               : 'vartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int list) in
    Obj.repr(
# 41 "parser.mly"
                          ( FuryList _1 )
# 230 "parser.ml"
               : 'vartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
                            ( FuryPlus (_1, _3) )
# 238 "parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                            ( FuryMinus (_1, _3) )
# 246 "parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                            ( FuryTimes (_1, _3) )
# 254 "parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                            ( FuryDivide (_1, _3) )
# 262 "parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                            ( -_2 )
# 269 "parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vartype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 53 "parser.mly"
                                        ( FuryEqualTo(_1, _3) )
# 277 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vartype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 54 "parser.mly"
                                        ( FuryMoreThan(_1, _3) )
# 285 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vartype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 55 "parser.mly"
                                        ( FuryEqualTo(_1, _3) )
# 293 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                           ( ( _2 ) )
# 300 "parser.ml"
               : 'bracketexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'conditional) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'numericaloperator) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'bracketexpr) in
    Obj.repr(
# 62 "parser.mly"
                                                       ( while _2 do _3 ; _4 done)
# 309 "parser.ml"
               : 'forloop))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'conditional) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                                                       ( FuryIf (_2, _3, _5 )
# 318 "parser.ml"
               : 'forloop))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 66 "parser.mly"
                                   ( read _2 )
# 325 "parser.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 67 "parser.mly"
                                   ( write _2 )
# 332 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Furyroad.furyterm)
