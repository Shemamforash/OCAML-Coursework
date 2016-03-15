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
  | FuryWrite of string
  | FuryDeclare of furytype * string * furyterm
  | FuryRebind of string * furyterm
  | FuryFor of furyterm * furyterm * (furyterm list)
  | FuryListDeclare of string
  | FuryAddToList of string * furyterm
  | FuryReplaceInList of string * furyterm
  | FuryGetFromList of string * furyterm
  | FuryIsListEmpty of string

type environment =
  | Environment of environment * ((string, furyprimitive) Hashtbl.t)
  | NullEnvironment

let stringtotype = function
  | "intperator" -> FINT
  | "war_rig" -> FLIST
  | _ -> print_string "waffle" ; failwith "Not a type";;

let typetostring = function
  | FBOOL -> "bool"
  | FINT -> "int"
  | FSTRING -> "string"
  | FVOID -> "void"
  | FLIST -> "list"
  | FFLOAT -> "float"

let rec listtostring = function
  | [] -> ""
  | hd :: [] -> string_of_int hd ^ ""
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
  | (FuryVar x) -> print_string ("variable \"" ^ x ^ "\" evaluates to ")
  | (FuryNegate e) -> print_string "negating " ; print_res e ; print_string " evaluates to -" ; print_res e
  | (FuryLessThan(e1, e2)) -> print_string "condition \"" ; print_res e1 ; print_string " < " ; print_res e2 ; print_string "\" evaluates to "
  | (FuryMoreThan(e1, e2)) -> print_string "condition \"" ; print_res e1 ; print_string " > " ; print_res e2 ; print_string "\" evaluates to "
  | (FuryEqualTo(e1, e2)) -> print_string "condition \"" ; print_res e1 ; print_string " == " ; print_res e2 ; print_string "\" evaluates to "
  | (FuryPlus(e1, e2)) -> print_string "operation \"" ; print_res e1 ; print_string " + " ; print_res e2 ; print_string "\" evaluates to "
  | (FuryMinus(e1, e2)) -> print_string "operation \"" ; print_res e1 ; print_string " - " ; print_res e2 ; print_string "\" evaluates to "
  | (FuryDivide(e1, e2)) -> print_string "operation \"" ; print_res e1 ; print_string " / " ; print_res e2 ; print_string "\" evaluates to "
  | (FuryTimes(e1, e2)) -> print_string "operation \"" ; print_res e1 ; print_string " x " ; print_res e2 ; print_string "\" evaluates to "
  | (FuryIf(e1,e2, e3)) -> print_string "if " ; print_res e1 ; print_string "true \n   then evaluate " ; exprlisttostring e2 ; print_string " \n   else evaluate " ; exprlisttostring e3
  | (FuryFor(e1, e2, e3)) -> print_string "start loop by " ; print_res e1 ; print_string "\n then while " ; print_res e2; print_string "\n is true, do " ; exprlisttostring e3
  | (FuryRead) -> print_string "reading stream"
  | (FuryWrite(n)) -> print_string ("writing " ^ n)
  | (FuryDeclare(e1, e2, e3)) -> print_string ("binding new variable \"" ^ e2 ^ "\" as ") ; print_res e3
  | (FuryRebind(e1, e2)) -> print_string ("rebinding " ^ e1 ^ " as ") ; print_res e2
  | (FuryListDeclare(e1)) -> print_string ("binding new list " ^ e1)
  | (FuryAddToList(e1, e2)) -> print_string "adding " ; print_res e2 ; print_string (" to list " ^ e1)
  | (FuryGetFromList(e1, e2)) -> print_string ("element in list " ^ e1 ^ " at position ") ; print_res e2 ; print_string " is "
  | (FuryIsListEmpty e1) -> print_string ("list " ^ e1 ^ " is empty? evaluates to ")
  | _ -> raise NonBaseTypeResult

and exprlisttostring = function
    | [] -> print_string "body end"
    | hd :: [] -> print_res hd ; print_string "\n"
    | hd :: tl -> print_res hd ; print_string "\n" ; exprlisttostring tl
