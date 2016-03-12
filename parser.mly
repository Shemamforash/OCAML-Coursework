/* File parser.mly */
%{
    open Furyroad
    open Types
%}

%token <int> INT
%token <string> VARIABLE
%token <bool> BOOL
%token <int list> LIST
%token <Types.furytype> TYPE
%token PLUS MINUS TIMES DIV EQUALS
%token LESSTHAN EQUALTO GREATERTHAN
%token LPAREN RPAREN
%token EOL
%token FORINIT FORCOND FORBODY READ WRITE IF ELSE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc LESSTHAN EQUALTO GREATERTHAN
%nonassoc UMINUS IF ELSE FOR READ WRITE    /* highest precedence */
%start main             /* the entry point */
%type <Types.out> main
%%

main:
    | EOL                     { Nothing }
    | expr EOL                { FuryTerm $1 }
;

expr:
    vartype                { $1 }
  | numericaloperator      { $1 }
  | conditional            { $1 }
  | bracketexpr            { $1 }
  | forloop                { $1 }
  | func                   { $1 }
  | declaration            { $1 }
;

sequence:
  | expr EOL sequence        { $1 :: $3 }
  | EOL sequence             { $2 }
  | expr                     { [$1] }
  |                          { [] }
;

vartype:
    INT                   { FuryInt $1 }
  | BOOL                  { FuryBool $1 }
  | LIST                  { FuryList $1 }
  | VARIABLE              { FuryVar $1 }
;

declaration:
  | TYPE VARIABLE EQUALS expr       { FuryDeclare($1, FuryString($2), $4)}
  | VARIABLE EQUALS expr            { FuryRebind(FuryString($1), $3) }
;

numericaloperator:
    expr PLUS expr          { FuryPlus ($1, $3) }
  | expr MINUS expr         { FuryMinus ($1, $3) }
  | expr TIMES expr         { FuryTimes ($1, $3) }
  | expr DIV expr           { FuryDivide ($1, $3) }
  | MINUS expr %prec UMINUS { FuryNegate $2 }
;

conditional:
    vartype LESSTHAN vartype            { FuryEqualTo($1, $3) }
  | vartype GREATERTHAN vartype         { FuryMoreThan($1, $3) }
  | vartype EQUALTO vartype             { FuryEqualTo($1, $3) }
;

bracketexpr:
  LPAREN expr RPAREN       { ( $2 ) }
;
forloop:
  | IF conditional sequence ELSE sequence                      { FuryIf ($2, $3, $5) }
  | FORINIT declaration FORCOND conditional FORBODY sequence                       { FuryFor ($2, $4, $6)}
;
func:
    READ                           { FuryRead }
  | WRITE vartype                  { FuryWrite $2 }
;
