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
%token LESSTHAN GREATERTHAN EQUALTO NEGATE
%token LISTADD LISTGET LISTREPLACE
%token LPAREN RPAREN
%token EOL BREAK
%token FORINIT FORCOND FORBODY READ WRITE IF THEN ELSE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc LESSTHAN EQUALTO GREATERTHAN BREAK NEGATE
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
  | listoperator           { $1 }
;

listoperator:
  | LISTADD VARIABLE expr        { FuryAddToList($2, $3) }
  | LISTGET VARIABLE expr        { FuryGetFromList($2, $3) }
  | LISTREPLACE VARIABLE expr    { FuryReplaceInList($2, $3) }
;

sequence:
  | expr BREAK sequence      { $1 :: $3 }
  | expr                     { [$1] }
  |                          { [] }
;

primitive:
  | INT                   { FuryInt $1 }
  | BOOL                  { FuryBool $1 }
  | FLOAT                 { FuryFloat $1 }
  | LIST                  { FuryList $1 }
;

declaration:
  | TYPE VARIABLE EQUALS expr        { FuryDeclare($1, FuryPrimitive(FuryString($2)), $4)}
  | VARIABLE EQUALS expr            { FuryRebind($1, $3) }
  | LIST VARIABLE                   { FuryListDeclare($2) }
;

numericaloperator:
  | expr PLUS expr          { FuryPlus ($1, $3) }
  | expr MINUS expr         { FuryMinus ($1, $3) }
  | expr TIMES expr         { FuryTimes ($1, $3) }
  | expr DIV expr           { FuryDivide ($1, $3) }
  | expr NEGATE %prec UMINUS { FuryNegate $1 }
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
  | IF conditional EOL THEN sequence EOL ELSE sequence                       { FuryIf ($2, $5, $8) }
  | IF conditional THEN sequence ELSE sequence                      { FuryIf ($2, $4, $6) }
  | FORINIT declaration EOL FORCOND conditional EOL FORBODY sequence   { FuryFor ($2, $5, $8)}
  | FORINIT declaration FORCOND conditional FORBODY sequence   { FuryFor ($2, $4, $6)}
;
func:
  | READ BREAK                          { FuryRead }
  | WRITE expr BREAK                    { FuryWrite $2 }
;
