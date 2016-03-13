(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
open Types
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
    | '='      { EQUALS }
    | "=="     { EQUALTO }
    | '<'      { LESSTHAN }
    | '>'      { GREATERTHAN }
    | ';'      { BREAK }
    | eof      { raise Eof }
    | "read"   { READ }
    | "write"  { WRITE }
    | "ilive" { FORINIT }
    | "idie"  { FORCOND }
    | "iliveagain" { FORBODY }
    | "if"     { IF }
    | "then"   { THEN }
    | "else"   { ELSE }
    | "int"  | "float" | "bool" as lxm      { TYPE(stringtotype lxm) }
    | ['a'-'z''A'-'Z']+ as lxm { VARIABLE(lxm) }
