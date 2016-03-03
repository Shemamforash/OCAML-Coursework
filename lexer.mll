(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule lexer = parse
      [' ' '\t']     { lexer lexbuf }     (* skip blanks *)
    | ['\n' ]  { EOL }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | '+'      { PLUS }
    | '-'      { MINUS }
    | '*'      { TIMES }
    | '/'      { DIV }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | eof      { raise Eof }
    | "read"   { READ }
    | "write"  { WRITE }
    | "for"    { FOR }
    | "if"     { IF }
    | "else"   { ELSE }
