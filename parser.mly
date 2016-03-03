/* File parser.mly */
%{
    open Functions
    open Furyroad
%}

%token <int> INT
%token <string> STRING
%token <bool> BOOL
%token <list> LIST
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
%type <int> main
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
    INT                   { INT $1 }
  | STRING                { STRING $1 }
  | BOOL                  { BOOL $1 }
;

numericaloperator:
    expr PLUS expr          { $1 + $3 }
  | expr MINUS expr         { $1 - $3 }
  | expr TIMES expr         { $1 * $3 }
  | expr DIV expr           { $1 / $3 }
  | MINUS expr %prec UMINUS { - $2 }
;

conditional:
    INT LESSTHAN INT            { ($1 < $3) }
  | INT GREATERTHAN INT         { ($1 > $3) }
  | INT EQUALTO INT             { ($1 = $3) }
;

bracketexpr:
  LPAREN expr RPAREN       { ( $2 ) }
;
forloop:
    FOR conditional numericaloperator bracketexpr      { while $2 do $3 ; $4 done}
  | IF conditional expr ELSE expr                      { if $2 then $3 else $5 }
;
func:
    READ INT                   { read $2 }
  | WRITE INT                  { write $2 }
;
