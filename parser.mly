/* File parser.mly */
%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token POINTYHAT MODULO
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS POINTYHAT MODULO      /* highest precedence */
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
 | expr POINTYHAT expr     { int_of_float ((float_of_int $1) ** (float_of_int $3)) }
 | expr MODULO expr        { $1 mod  $3}
 | MINUS expr %prec UMINUS { - $2 }
;
