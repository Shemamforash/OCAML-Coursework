(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
open Types
exception Eof
}
rule lexer = parse
      [' ' '\t']     { lexer lexbuf }             (* skip blanks *)
    | ['\n' ]                                     { EOL }
    | ['0'-'9']+ as lxm                           { INT(int_of_string lxm) }
    | '?'                                         { QUESTION }
    | eof      { raise Eof }
    | "intperator" as lxm                         { TYPE(stringtotype lxm) }
    | ['a'-'z''A'-'Z']+ as lxm                    { VARIABLE(lxm) }

    (* mathematical operators *)
    | "risen_by"                                  { PLUS }
    | "lowered_by"                                { MINUS }
    | "powered_by"                                { TIMES }
    | "slashed_by"                                { DIV }
    | "reversed"                                  { NEGATE }
    | "becomes"                                   { EQUALS }

    (* conditional operators *)
    | "stands_on_equal_ground_with"               { EQUALTO }
    | "is_warboy_to"                              { LESSTHAN }
    | "is_immortan_to"                            { GREATERTHAN }
    | "V8"                                        { BREAK }

    (* list operators *)
    | "war_party"                                 { LIST }
    | "ride_with"                                 { LISTADD }
    | "find_in"                                   { LISTGET }
    | "is_war_party_disbanded"                    { LISTEMPTY }

    (* IO tokens *)
    | "witness"                                   { WRITE }
    | "read"                                      { READ }

    (* loop/if tokens *)
    | "i_live"                                    { FORINIT }
    | "i_die"                                     { FORCOND }
    | "i_live_again"                              { FORBODY }
    | "am_i_awaited"                              { IF }
    | "then_ride_eternal_shiny_and_chrome"        { THEN }
    | "or_die_historic_on_fury_road"              { ELSE }
