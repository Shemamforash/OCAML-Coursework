/* File parser.mly */
%{
    open Furyroad
    open Types
%}

%token <int> INT
%token <string> VARIABLE
%token <bool> BOOL
%token <float> FLOAT
%token <int list> LIST
%token <Types.furytype> TYPE
%token PLUS MINUS TIMES DIV EQUALS
%token LESSTHAN GREATERTHAN EQUALTO
%token LPAREN RPAREN
%token EOL BREAK
%token FORINIT FORCOND FORBODY READ WRITE IF THEN ELSE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc LESSTHAN EQUALTO GREATERTHAN BREAK
%nonassoc UMINUS IF THEN ELSE FORINIT FORCOND FORBODY READ WRITE    /* highest precedence */
%start main             /* the entry point */
%type <Types.out> main
%%

main:
    | EOL                     { Nothing }
    | expr EOL                { FuryTerm $1 }
;

expr:
  | primitive              { FuryPrimitive($1) }
  | numericaloperator      { $1 }
  | conditional            { $1 }
  | bracketexpr            { $1 }
  | forloop                { $1 }
  | func                   { $1 }
  | declaration            { $1 }
  | VARIABLE               { FuryVar $1 }
;

sequence:
  | expr BREAK sequence      { $1 :: $3 }
  | expr                     { [$1] }
  |                          { [] }
;

primitive:
  | INT                   { FuryInt $1 }
  | BOOL                  { FuryBool $1 }
  | LIST                  { FuryList $1 }
  | FLOAT                 { FuryFloat $1 }
;

declaration:
  | TYPE VARIABLE EQUALS expr       { FuryDeclare($1, FuryPrimitive(FuryString($2)), $4)}
  | VARIABLE EQUALS expr            { FuryRebind($1, $3) }
;

numericaloperator:
  | expr PLUS expr          { FuryPlus ($1, $3) }
  | expr MINUS expr         { FuryMinus ($1, $3) }
  | expr TIMES expr         { FuryTimes ($1, $3) }
  | expr DIV expr           { FuryDivide ($1, $3) }
  | MINUS expr %prec UMINUS { FuryNegate $2 }
;

conditional:
  | expr LESSTHAN expr            { FuryLessThan($1, $3) }
  | expr GREATERTHAN expr         { FuryMoreThan($1, $3) }
  | expr EQUALTO expr              { FuryEqualTo($1, $3) }
;

bracketexpr:
  | LPAREN expr RPAREN       { ( $2 ) }
;
forloop:
  | IF LPAREN conditional RPAREN EOL THEN LPAREN sequence RPAREN EOL ELSE LPAREN sequence RPAREN                      { FuryIf ($3, $8, $13) }
  | IF LPAREN conditional RPAREN THEN LPAREN sequence RPAREN ELSE LPAREN sequence RPAREN                      { FuryIf ($3, $7, $11) }
  | FORINIT LPAREN declaration RPAREN EOL FORCOND LPAREN conditional RPAREN EOL FORBODY LPAREN sequence RPAREN   { FuryFor ($3, $8, $13)}
  | FORINIT LPAREN declaration RPAREN FORCOND LPAREN conditional RPAREN FORBODY LPAREN sequence RPAREN   { FuryFor ($3, $7, $11)}
;
func:
  | READ BREAK                          { FuryRead }
  | WRITE expr BREAK                    { FuryWrite $2 }
;
