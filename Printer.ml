open Types;;
open Exceptions;;

let stringtotype = function
  | "intperator" -> FINT
  | "war_party" -> FLIST
  | _ -> raise TypeError;;

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

let rec print_res res = if viewtrace then (match res with
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
  | (FuryRead) -> print_string ("reading line from stdin")
  | (FuryWrite(n)) -> print_string ("writing ") ; print_res n
  | (FuryDeclareAndBind(e1, e2, e3)) -> print_string ("binding new variable \"" ^ e2 ^ "\" as ") ; print_res e3
  | (FuryDeclare(e1, e2)) -> print_string ("declaring new " ^ typetostring e1 ^ " as \"" ^ e2)
  | (FuryRebind(e1, e2)) -> print_string ("rebinding " ^ e1 ^ " as ") ; print_res e2
  | (FuryAddToList(e1, e2)) -> print_string "adding " ; print_res e2 ; print_string (" to list " ^ e1)
  | (FuryGetFromList(e1, e2)) -> print_string ("element in list " ^ e1 ^ " at position ") ; print_res e2 ; print_string " is "
  | (FuryIsListEmpty e1) -> print_string ("list " ^ e1 ^ " is empty? evaluates to ")
  | _ -> raise NonBaseTypeResult) else print_string ""

and exprlisttostring = function
    | [] -> print_string "body end"
    | hd :: [] -> print_res hd ; print_string "\n"
    | hd :: tl -> print_res hd ; print_string "\n" ; exprlisttostring tl
