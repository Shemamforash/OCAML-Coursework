/* File parser.mly */
%{
    open Furyroad
    open Types
%}

%token <int> INT
%token <string> VARIABLE
%token LIST
%token <Types.furytype> TYPE
%token PLUS MINUS TIMES DIV EQUALS NEGATE
%token LESSTHAN GREATERTHAN EQUALTO
%token LISTADD LISTGET LISTEMPTY
%token EOL BREAK QUESTION
%token FORINIT FORCOND FORBODY
%token READ WRITE
%token IF THEN ELSE

%left EQUALTO LESSTHAN GREATERTHAN    /* lowest precedence */
%left PLUS MINUS                      /* medium precedence */
%left TIMES DIV                       /* highest precedence */

%nonassoc UMINUS

%start main /* the entry point */

%type <Types.out> main
%%

main:
    | EOL                                                                                  { Nothing }
    | expr EOL                                                                             { FuryTerm $1 }
;

expr:
  | primitives                                                                             { $1 }
  | conditional                                                                            { $1 }
  | forloop                                                                                { $1 }
  | func                                                                                   { $1 }
  | declaration                                                                            { $1 }
  | listoperator                                                                           { $1 }
;

listoperator:
  | LISTADD VARIABLE numericaloperator                                                     { FuryAddToList($2, $3) }
  | LISTGET VARIABLE numericaloperator                                                     { FuryGetFromList($2, $3) }
  | LISTEMPTY VARIABLE QUESTION                                                            { FuryIsListEmpty $2 }
;

sequence:
  | expr BREAK sequence                                                                    { $1 :: $3 }
  | expr                                                                                   { [$1] }
  |                                                                                        { [] }
;

declaration:
  | TYPE VARIABLE EQUALS numericaloperator                                                 { FuryDeclare($1, $2, $4) }
  | VARIABLE EQUALS numericaloperator                                                      { FuryRebind($1, $3) }
  | LIST VARIABLE                                                                          { FuryListDeclare($2) }
;

primitives:
  | VARIABLE                                                                               { FuryVar $1}
  | INT                                                                                    { FuryPrimitive(FuryInt $1) }
  | numericaloperator                                                                      { $1 }
;

numericaloperator:
  | primitives PLUS primitives                                                             { FuryPlus ($1, $3) }
  | primitives MINUS primitives                                                            { FuryMinus ($1, $3) }
  | primitives TIMES primitives                                                            { FuryTimes ($1, $3) }
  | primitives DIV primitives                                                              { FuryDivide ($1, $3) }
  | primitives NEGATE %prec UMINUS                                                         { FuryNegate $1 }
;

conditional:
  | primitives LESSTHAN primitives QUESTION                                                { FuryLessThan($1, $3) }
  | primitives GREATERTHAN primitives QUESTION                                             { FuryMoreThan($1, $3) }
  | primitives EQUALTO primitives QUESTION                                                 { FuryEqualTo($1, $3) }
;

forloop:
  | IF EOL conditional EOL THEN EOL sequence EOL ELSE EOL sequence                         { FuryIf ($3, $7, $11) }
  | FORINIT EOL declaration EOL FORCOND EOL conditional EOL FORBODY EOL sequence           { FuryFor($3, $7, $11)}
;
func:
  | READ                                                                                   { FuryRead }
  | WRITE VARIABLE                                                                         { FuryWrite $2 }
;
