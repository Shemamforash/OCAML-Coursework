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

open Parsing;;
let _ = parse_error;;
# 3 "Parser.mly"
    open Furyroad
# 29 "Parser.ml"
let yytransl_const = [|
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIV *);
  266 (* EQUALS *);
  267 (* LESSTHAN *);
  268 (* EQUALTO *);
  269 (* GREATERTHAN *);
  270 (* LPAREN *);
  271 (* RPAREN *);
  272 (* EOL *);
  273 (* FOR *);
  274 (* READ *);
  275 (* WRITE *);
  276 (* IF *);
  277 (* ELSE *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VARIABLE *);
  259 (* BOOL *);
  260 (* LIST *);
  261 (* TYPE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\009\000\004\000\004\000\004\000\004\000\004\000\
\005\000\005\000\005\000\006\000\007\000\008\000\008\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\004\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\003\000\003\000\005\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\010\000\000\000\000\000\022\000\
\000\000\000\000\024\000\000\000\000\000\003\000\004\000\005\000\
\006\000\007\000\016\000\000\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\020\000\
\000\000\000\000\000\000\014\000\015\000\017\000\019\000\018\000\
\000\000\021\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\014\000\015\000\016\000\017\000\018\000\
\000\000"

let yysindex = "\003\000\
\024\255\000\000\000\000\000\000\000\000\024\255\024\255\000\000\
\029\255\029\255\000\000\255\254\028\255\000\000\000\000\000\000\
\000\000\000\000\000\000\062\255\000\000\028\255\024\255\024\255\
\024\255\024\255\024\255\000\000\029\255\029\255\029\255\000\000\
\005\255\001\255\001\255\000\000\000\000\000\000\000\000\000\000\
\024\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\043\255\046\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\249\255\000\000\006\000\000\000\000\000\000\000\
\000\000"

let yytablesize = 77
let yytable = "\019\000\
\020\000\021\000\022\000\001\000\024\000\025\000\026\000\027\000\
\026\000\027\000\024\000\025\000\026\000\027\000\028\000\023\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\003\000\041\000\004\000\005\000\000\000\003\000\006\000\004\000\
\005\000\000\000\042\000\000\000\000\000\007\000\029\000\030\000\
\031\000\008\000\009\000\010\000\002\000\002\000\002\000\002\000\
\012\000\012\000\000\000\013\000\013\000\002\000\002\000\000\000\
\000\000\012\000\012\000\002\000\013\000\013\000\000\000\012\000\
\000\000\000\000\013\000\024\000\025\000\026\000\027\000\000\000\
\000\000\000\000\000\000\000\000\032\000"

let yycheck = "\006\000\
\007\000\009\000\010\000\001\000\006\001\007\001\008\001\009\001\
\008\001\009\001\006\001\007\001\008\001\009\001\016\001\010\000\
\023\000\024\000\025\000\026\000\027\000\029\000\030\000\031\000\
\001\001\021\001\003\001\004\001\255\255\001\001\007\001\003\001\
\004\001\255\255\041\000\255\255\255\255\014\001\011\001\012\001\
\013\001\018\001\019\001\020\001\006\001\007\001\008\001\009\001\
\006\001\007\001\255\255\006\001\007\001\015\001\016\001\255\255\
\255\255\015\001\016\001\021\001\015\001\016\001\255\255\021\001\
\255\255\255\255\021\001\006\001\007\001\008\001\009\001\255\255\
\255\255\255\255\255\255\255\255\015\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  EQUALS\000\
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
  VARIABLE\000\
  BOOL\000\
  LIST\000\
  TYPE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 25 "Parser.mly"
                            ( _1 )
# 158 "Parser.ml"
               : Furyroad.furyterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 29 "Parser.mly"
                           ( _1 )
# 165 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'numericaloperator) in
    Obj.repr(
# 30 "Parser.mly"
                           ( _1 )
# 172 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conditional) in
    Obj.repr(
# 31 "Parser.mly"
                           ( _1 )
# 179 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bracketexpr) in
    Obj.repr(
# 32 "Parser.mly"
                           ( _1 )
# 186 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'forloop) in
    Obj.repr(
# 33 "Parser.mly"
                           ( _1 )
# 193 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func) in
    Obj.repr(
# 34 "Parser.mly"
                           ( _1 )
# 200 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 38 "Parser.mly"
                          ( FuryInt _1 )
# 207 "Parser.ml"
               : 'vartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 39 "Parser.mly"
                          ( FuryBool _1 )
# 214 "Parser.ml"
               : 'vartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int list) in
    Obj.repr(
# 40 "Parser.mly"
                          ( FuryList _1 )
# 221 "Parser.ml"
               : 'vartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : FuryRoad.furytype) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 44 "Parser.mly"
                                     ( FuryDeclare(_1, FuryString _2, _4))
# 230 "Parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "Parser.mly"
                            ( FuryPlus (_1, _3) )
# 238 "Parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "Parser.mly"
                            ( FuryMinus (_1, _3) )
# 246 "Parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "Parser.mly"
                            ( FuryTimes (_1, _3) )
# 254 "Parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "Parser.mly"
                            ( FuryDivide (_1, _3) )
# 262 "Parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "Parser.mly"
                            ( FuryNegate (_2) )
# 269 "Parser.ml"
               : 'numericaloperator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vartype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 56 "Parser.mly"
                                        ( FuryEqualTo(_1, _3) )
# 277 "Parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vartype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 57 "Parser.mly"
                                        ( FuryMoreThan(_1, _3) )
# 285 "Parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vartype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 58 "Parser.mly"
                                        ( FuryEqualTo(_1, _3) )
# 293 "Parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 62 "Parser.mly"
                           ( ( _2 ) )
# 300 "Parser.ml"
               : 'bracketexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'conditional) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "Parser.mly"
                                                       ( FuryIf (_2, _3, _5) )
# 309 "Parser.ml"
               : 'forloop))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "Parser.mly"
                                   ( FuryRead )
# 315 "Parser.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vartype) in
    Obj.repr(
# 69 "Parser.mly"
                                   ( FuryWrite _2 )
# 322 "Parser.ml"
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
