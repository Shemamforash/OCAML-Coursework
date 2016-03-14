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
    | "risen_by"      { PLUS }
    | "lowered_by"      { MINUS }
    | '*'      { TIMES }
    | '/'      { DIV }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | "becomes"      { EQUALS }
    | "=="     { EQUALTO }
    | '<'      { LESSTHAN }
    | '>'      { GREATERTHAN }
    | ';'      { BREAK }
    | "war_rig" { LIST[] }
    | "repair" { LISTREPLACE }
    | "improve" { LISTADD }
    | "find_in" { LISTGET }
    | "reversed" { NEGATE }
    | eof      { raise Eof }
    | "witness" { WRITE }
    | "read"   { READ }
    | "i_live" { FORINIT }
    | "i_die"  { FORCOND }
    | "i_live_again" { FORBODY }
    | "am_i_awaited"     { IF }
    | "then_ride_eternal_shiny_and_chrome"   { THEN }
    | "or_die_historic_on_fury_road"   { ELSE }
    | "intperator" as lxm { TYPE(stringtotype lxm) }
    | ['a'-'z''A'-'Z']+ as lxm { VARIABLE(lxm) }
