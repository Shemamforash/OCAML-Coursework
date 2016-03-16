open Furyroad;;
open Types;;
open Lexer;;
open Parser;;
open Printf;;
open Scanf;;
open Printer;;
open Exceptions;;

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
                    | FuryNull -> print_string ""
                    | _ -> print_primitive evaluated ; print_string "\n" ; flush stdout)
        | Nothing -> ()
      with
          | Parsing.Parse_error -> Printf.eprintf "%s\n" "Parse error (maybe you mispelled something?) at " ; flush stderr
          | TypeError -> Printf.eprintf "%s\n" "Bad type used" ; flush stderr
          | DivideByZero -> Printf.eprintf "%s\n" "Divide by zero" ; flush stderr
          | RootEnvironmentLeft -> Printf.eprintf "%s\n" "Unbound variable used" ; flush stderr
          | TypeMismatch -> Printf.eprintf "%s\n" "Variable types do not match" ; flush stderr
          | VariableAlreadyDeclared -> Printf.eprintf "%s\n" "Variable has already been declared" ; flush stderr
          | OutOfBounds -> Printf.eprintf "%s\n" "List access out of bounds" ; flush stderr
          | NonBaseTypeResult -> Printf.eprintf "%s\n" "Tried to evaluate bad expression." ; flush stderr
    done
  ) with
  | Lexer.Eof ->  flush stdout ; exit 0;
