/* File parser.mly */
%token <int> INT
%token <string> STRING
%token <bool> BOOL
%token PLUS MINUS TIMES DIV
%token LESSTHAN EQUALTO GREATERTHAN
%token LPAREN RPAREN
%token EOL
%token FOR READ WRITE
%left LESSTHAN EQUALTO GREATERTHAN
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
   type                     { $1 }
   | numericaloperator      { $1 }
   | conditional            { $1 }
   | bracketexpr            { $1 }
   | forloop                { $1 }
   | func                   { $1 }
;
type:
  INT { 'INT $1 }
  | STRING { 'STRING $1 }
  | BOOL { 'BOOL $1 }
;
numericaloperator:
 expr PLUS expr          { $1 + $3 }
| expr MINUS expr         { $1 - $3 }
| expr TIMES expr         { $1 * $3 }
| expr DIV expr           { $1 / $3 }
| MINUS expr %prec UMINUS { - $2 }
;
conditional:
   INT LESSTHAN INT        { ($1 < $3) }
 | INT GREATERTHAN INT     { ($1 > $3) }
 | INT EQUALTO INT         { ($1 = $3) }
;
bracketexpr:
  LPAREN expr RPAREN       { ( $2 ) }
;
forloop:
| FOR conditional numericaloperator bracketexpr      { while $2 do $3 $4}
;
func:
  READ INT                   {let read (pos : int) : int list = let infile = open_in Sys.argv.(1) in let columnarr = ref [] in try while (true) do let line = input_line infile in let num = int_of_string (String.make 1 line.[$2*2]) in columnarr := num::!columnarr; done; !columnarr with e -> close_in infile; List.rev !columnarr;;}
  | WRITE INT                  { let write val = Printf.printf "%d" val;; }
;
