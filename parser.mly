/* File parser.mly */
%token <int> INT
%token LIST
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token FOR READ WRITE
%token <bool> LESSTHAN EQUALTO GREATERTHAN
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left LESSTHAN EQUALTO GREATERTHAN
%nonassoc UMINUS     /* highest precedence */
%start main             /* the entry point */
%type <int> main
%type <bool> cond
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
 | FOR cond expr bracketexpr      { while $2 do $3 $4}
;
bracketexpr:
  LPAREN expr RPAREN       { ( $2 ) }
;
cond:
   INT LESSTHAN INT        { ($1 < $3) }
 | INT GREATERTHAN INT     { ($1 > $3) }
 | INT EQUALTO INT         { ($1 = $3) }
;
func:
  READ INT                   {let read (pos : int) : int list = let infile = open_in Sys.argv.(1) in let columnarr = ref [] in try while (true) do let line = input_line infile in let num = int_of_string (String.make 1 line.[$2*2]) in columnarr := num::!columnarr; done; !columnarr with e -> close_in infile; List.rev !columnarr;;}
  | WRITE INT                  { let write val = Printf.printf "%d" val;; }
;
