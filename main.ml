open Furyroad
open Functions
open Lexer
open Parser
open Arg
open Printf

let parseProgram c =
    try let lexbuf = Lexing.from_channel c in
            main lexer lexbuf
    with Parsing.Parse_error -> failwith "Parse failure!" ;;


let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./main PROGRAM_FILE" in
parse [] setProg usage ;
let parsedProg = parseProgram !arg in
let () = print_string "Program Parsed" ; print_newline() in
let _ = typeProg parsedProg in
let () = print_string "Program Type Checked" ; print_newline() in
let result1 = evalProg parsedProg in
flush stdout
