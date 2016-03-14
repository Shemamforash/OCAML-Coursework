exception NonBaseTypeResult;;

type furytype = FINT | FLIST | FBOOL | FSTRING | FVOID | FFLOAT

type out = Nothing | FuryTerm of furyterm

and furyprimitive =
  | FuryInt of int
  | FuryBool of bool
  | FuryList of int list
  | FuryString of string
  | FuryFloat of float
  | FuryVoid of furyterm
  | FuryNull

and furyterm =
  | FuryPrimitive of furyprimitive
  | FuryLessThan of furyterm * furyterm
  | FuryMoreThan of furyterm * furyterm
  | FuryEqualTo of furyterm * furyterm
  | FuryPlus of furyterm * furyterm
  | FuryMinus of furyterm * furyterm
  | FuryDivide of furyterm * furyterm
  | FuryTimes of furyterm * furyterm
  | FuryNegate of furyterm
  | FuryIf of furyterm * (furyterm list) * (furyterm list)
(*  | FuryFor of furyterm * furyterm * furyterm  *)
  | FuryVar of string
  | FuryRead
  | FuryWrite of furyterm
  | FuryDeclare of furytype * furyterm * furyterm
  | FuryRebind of string * furyterm
  | FuryFor of furyterm * furyterm * (furyterm list)

type environment =
  | Environment of environment * ((string, furyprimitive) Hashtbl.t)
  | NullEnvironment

let stringtotype = function
  | "booletfarm" -> FBOOL
  | "intperator" -> FINT
  | "war_rig" -> FLIST
  | "float" -> FFLOAT
  | _ -> failwith "Not a type";;

let typetostring = function
  | FBOOL -> "bool"
  | FINT -> "int"
  | FSTRING -> "string"
  | FVOID -> "void"
  | FLIST -> "list"
  | FFLOAT -> "float"

let rec listtostring = function
  | [] -> ""
  | hd :: [] -> string_of_int hd ^ ", "
  | hd :: tl -> string_of_int hd ^ ", " ^ listtostring tl

let print_primitive p = match p with
  | (FuryInt i) -> print_int i
  | (FuryBool b) -> print_string (if b then "true" else "false")
  | (FuryString s) -> print_string s
  | (FuryList l) ->  print_string (listtostring l ^ " : List")
  | (FuryVoid v) -> print_string "Void"
  | (FuryFloat f) -> print_float f
  | (FuryNull) -> print_string "Null"

let rec print_res res = match res with
  | (FuryPrimitive p) -> print_primitive p
  | (FuryVar x) -> print_string (x ^ " x = ")
  | (FuryNegate e) -> print_string "-" ; print_res e
  | (FuryLessThan(e1, e2)) -> print_res e1 ; print_string " < " ; print_res e2
  | (FuryMoreThan(e1, e2)) -> print_res e1 ; print_string " > " ; print_res e2
  | (FuryEqualTo(e1, e2)) -> print_res e1 ; print_string " == " ; print_res e2
  | (FuryPlus(e1, e2)) -> print_res e1 ; print_string " + " ; print_res e2
  | (FuryMinus(e1, e2)) -> print_res e1 ; print_string " - " ; print_res e2
  | (FuryDivide(e1, e2)) -> print_res e1 ; print_string " / " ; print_res e2
  | (FuryTimes(e1, e2)) -> print_res e1 ; print_string " x " ; print_res e2
  | (FuryIf(e1,e2, e3)) -> print_string "if " ; print_res e1 ; print_string " then " ; exprlisttostring e2 ; print_string " else " ; exprlisttostring e3
  | (FuryFor(e1, e2, e3)) -> print_string "for " ; print_res e1 ; print_string " while " ; print_res e2; print_string " do " ; exprlisttostring e3
  | (FuryRead) -> print_string "reading stream"
  | (FuryWrite(n)) -> print_res n ; print_string "writing out"
  | (FuryDeclare(e1, e2, e3)) -> print_res e2 ; print_string " = " ; print_res e3
  | (FuryRebind(e1, e2)) -> print_string (e1 ^ " = ") ; print_res e2
  | _ -> raise NonBaseTypeResult

and exprlisttostring = function
    | [] -> print_string "body end"
    | hd :: [] -> print_res hd
    | hd :: tl -> print_res hd ; exprlisttostring tl
