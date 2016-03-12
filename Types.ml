type furytype = FINT | FLIST | FBOOL | FSTRING | FVOID

type furyterm =
    FuryInt of int
  | FuryBool of bool
  | FuryList of int list
  | FuryString of string
  | FuryLessThan of furyterm * furyterm
  | FuryMoreThan of furyterm * furyterm
  | FuryEqualTo of furyterm * furyterm
  | FuryPlus of furyterm * furyterm
  | FuryMinus of furyterm * furyterm
  | FuryDivide of furyterm * furyterm
  | FuryTimes of furyterm * furyterm
  | FuryNegate of furyterm
  | FuryIf of furyterm * furyterm * furyterm
(*  | FuryFor of furyterm * furyterm * furyterm  *)
  | FuryVar of string
  | FuryRead
  | FuryWrite of furyterm
  | FuryDeclare of furytype * furyterm * furyterm
  | FuryVoid of unit

type out = Nothing | FuryTerm of furyterm

type environment =
  | Environment of environment * ((string, furyterm) Hashtbl.t)
  | NullEnvironment

let stringtotype = function
  | "bool" -> FBOOL
  | "int" -> FINT
  | "list" -> FLIST
  | _ -> failwith "Not a type";;

let typetostring = function
  | FBOOL -> "bool"
  | FINT -> "int"
  | FSTRING -> "string"
  | FVOID -> "void"
  | FLIST -> "list"
  | _ -> failwith "Not a type";;

let listtostring = function
  | [] -> ""
  | hd :: [] -> string_of_int hd ^ ", "
  | hd :: tl -> string_of_int hd ^ ", " ^ listtostring tl

let print_res res = match res with
  | (FuryInt i) -> print_int i ; print_string " : Int"
  | (FuryBool b) -> print_string (if b then "true" else "false") ; print_string " : Bool"
  | (FuryVar x) -> print_string x " : Variable"
  | (FuryString s) -> print_string s " : String"
  | (FuryList l) -> listtostring l ; print_string " : List"
  | (FuryVoid v) -> print_string " : Void"
  | (FuryNegate e) -> print_string "-" ; print_res e
  | (FuryLessThan(e1, e2)) -> print_res e1 ; print_string " < " ; print_res e2
  | (FuryMoreThan(e1, e2)) -> print_res e1 ; print_string " > " ; print_res e2
  | (FuryEqualTo(e1, e2)) -> print_res e1 ; print_string " == " ; print_res e2
  | (FuryPlus(e1, e2)) -> print_res e1 ; print_string " + " ; print_res e2
  | (FuryMinus(e1, e2)) -> print_res e1 ; print_string " - " ; print_res e2
  | (FuryDivide(e1, e2)) -> print_res e1 ; print_string " / " ; print_res e2
  | (FuryTimes(e1, e2)) -> print_res e1 ; print_string " x " ; print_res e2
  | (FuryIf(e1,e2, e3)) -> print_string "if " ; print_res e1 ; print_string " then " ; print_res e2 ; print_string " else " ; print_res e3
  | (FuryRead) -> print_string "reading stream"
  | (FuryWrite(FuryInt(n))) -> print_string "writing out"
  | (FuryDeclare(e1, e2, e3)) -> print_string e2 " = " ; print_res e3
  | _ -> raise NonBaseTypeResult
