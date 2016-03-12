open Furyroad
open Functions
open Types
open Lexer
open Parser
open Printf

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
        | FuryTerm e -> let evaluated = (evaluate root e) in print_res evaluated ; print_string "\n" ; flush stdout
        | Nothing -> ()
      with
      | Parsing.Parse_error -> failwith "Parse failure!"
    done
  ) with
  | Lexer.Eof ->  flush stdout ; exit 0;
