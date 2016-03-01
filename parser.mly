/* File parser.mly */
%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token FOR
%token <bool> LESSTHAN EQUALTO GREATERTHAN
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS     /* highest precedence */
%start main             /* the entry point */
%type <int> main
%%
main:
   expr EOL                { $1 }
;
expr:
   INT                     { $1 }
 | LPAREN expr RPAREN      { $2 }
 | expr PLUS expr          { $1 + $3 }
 | expr MINUS expr         { $1 - $3 }
 | expr TIMES expr         { $1 * $3 }
 | expr DIV expr           { $1 / $3 }
 | MINUS expr %prec UMINUS { - $2 }
 | FOR cond expr expr      { while $1 do $2 $3}

;
cond:
   INT LESSTHAN INT        { $1 < $3 }
 | INT GREATERTHAN INT     { $1 > $3 }
 | INT EQUALTO INT         { $1 == $3 }
;
