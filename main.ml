open Furyroad
open Types
open Lexer
open Parser
open Printf
open Scanf

let program_file = open_in Sys.argv.(1);;

let voidEnv = NullEnvironment;;
let root = Environment (voidEnv, Hashtbl.create(5));;

let arg =
  try (
    let lexbuf = Lexing.from_channel program_file
    in while true do
      try
        let result = main lexer lexbuf
        in match result with
        | FuryTerm e -> let evaluated = (evaluate root e) in (match evaluated with
                    | FuryNull -> print_string "\n\n"
                    | _ -> print_primitive evaluated ; print_string "\n\n" ; flush stdout)
        | Nothing -> ()
      with
      | Parsing.Parse_error -> (let curr = lexbuf.Lexing.lex_curr_p in
            let line = curr.Lexing.pos_lnum in
              let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
                let tok = Lexing.lexeme lexbuf in
                  failwith ("Parsing error at: line" ^ (string_of_int line) ^ " character " ^ (string_of_int cnum) ^ " token " ^ tok))
    done
  ) with
  | Lexer.Eof ->  flush stdout ; exit 0;
