/* File parser.mly */
%{
    open Functions
    open Furyroad
%}

%token <int> INT
%token <string> STRING
%token <bool> BOOL
%token <int list> LIST
%token PLUS MINUS TIMES DIV
%token LESSTHAN EQUALTO GREATERTHAN
%token LPAREN RPAREN
%token EOL
%token FOR READ WRITE IF ELSE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc LESSTHAN EQUALTO GREATERTHAN
%nonassoc UMINUS IF ELSE FOR READ WRITE    /* highest precedence */
%start main             /* the entry point */
%type <Furyroad.furyterm> main
%%

main:
    expr EOL                { $1 }
;

expr:
    vartype                { $1 }
  | numericaloperator      { $1 }
  | conditional            { $1 }
  | bracketexpr            { $1 }
  | forloop                { $1 }
  | func                   { $1 }
;

vartype:
    INT                   { FuryInt $1 }
  | STRING                { FuryString $1 }
  | BOOL                  { FuryBool $1 }
  | LIST                  { FuryList $1 }
;

numericaloperator:
    expr PLUS expr          { FuryPlus ($1, $3) }
  | expr MINUS expr         { FuryMinus ($1, $3) }
  | expr TIMES expr         { FuryTimes ($1, $3) }
  | expr DIV expr           { FuryDivide ($1, $3) }
  | MINUS expr %prec UMINUS { -$2 }
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
    FOR conditional numericaloperator bracketexpr      { while $2 do $3 ; $4 done}
  | IF conditional expr ELSE expr                      { FuryIf ($2, $3, $5 }
;
func:
    READ vartype                   { read $2 }
  | WRITE vartype                  { write $2 }
;
