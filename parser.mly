/* File parser.mly */
%{
    open Furyroad
    open Types
%}

%token <int> INT
%token <string> VARIABLE
%token LIST
%token <Types.furytype> TYPE
%token PLUS MINUS TIMES DIV EQUALS NEGATESTART NEGATEEND
%token LESSTHAN GREATERTHAN EQUALTO
%token LISTADD LISTGET LISTEMPTY
%token EOL BREAK QUESTION
%token FORINIT FORCOND FORBODY
%token READ WRITE
%token IF THEN ELSE LP RP

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
  | LISTADD VARIABLE primitives                                                            { FuryAddToList($2, $3) }
  | LISTGET VARIABLE primitives                                                            { FuryGetFromList($2, $3) }
;

sequence:
  | expr BREAK EOL sequence                                                                { $1 :: $4 }
  | expr                                                                                   { [$1] }
  |                                                                                        { [] }
;

declaration:
  | TYPE VARIABLE EQUALS primitives                                                        { FuryDeclare($1, $2, $4) }
  | VARIABLE EQUALS numericaloperator                                                      { FuryRebind($1, $3) }
  | LIST VARIABLE                                                                          { FuryListDeclare($2) }
  | LIST VARIABLE EQUALS READ                                                              { FuryListDeclareWithRead($2) }
;

primitives:
  | VARIABLE                                                                               { FuryVar $1}
  | INT                                                                                    { FuryPrimitive(FuryInt $1) }
  | numericaloperator                                                                      { $1 }
  | listoperator                                                                           { $1 }
;

numericaloperator:
  | primitives PLUS primitives                                                             { FuryPlus ($1, $3) }
  | primitives MINUS primitives                                                            { FuryMinus ($1, $3) }
  | primitives TIMES primitives                                                            { FuryTimes ($1, $3) }
  | primitives DIV primitives                                                              { FuryDivide ($1, $3) }
  | NEGATESTART primitives NEGATEEND %prec UMINUS                                          { FuryNegate $2 }
;

conditional:
  | primitives LESSTHAN primitives QUESTION                                                { FuryLessThan($1, $3) }
  | primitives GREATERTHAN primitives QUESTION                                             { FuryMoreThan($1, $3) }
  | primitives EQUALTO primitives QUESTION                                                 { FuryEqualTo($1, $3) }
  | LISTEMPTY VARIABLE QUESTION                                                            { FuryIsListEmpty $2 }
;

forloop:
  | IF EOL conditional EOL THEN EOL sequence EOL ELSE EOL sequence                         { FuryIf ($3, $7, $11) }
  | FORINIT EOL declaration EOL FORCOND EOL conditional EOL FORBODY EOL sequence           { FuryFor($3, $7, $11)}
;

func:
  | VARIABLE EQUALS READ                                                                   { FuryRead $1 }
  | WRITE VARIABLE                                                                         { FuryWrite $2 }
;
