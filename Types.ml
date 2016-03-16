let viewtrace = false;;

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
  | FuryDeclare of furytype * string
  | FuryDeclareAndBind of furytype * string * furyterm
  | FuryRebind of string * furyterm
  | FuryFor of furyterm * furyterm * (furyterm list)
  | FuryAddToList of string * furyterm
  | FuryReplaceInList of string * furyterm
  | FuryGetFromList of string * furyterm
  | FuryIsListEmpty of string

type environment =
  | Environment of environment * ((string, (furytype * furyprimitive)) Hashtbl.t)
  | NullEnvironment

let typeofprimitive p = match p with
  |FuryInt (n) -> FINT
  |FuryBool (b) -> FBOOL
  |FuryList (l) -> FLIST
  |FuryString (s) -> FSTRING
  |FuryVoid(u) -> FVOID
  |FuryFloat(f) -> FFLOAT
  |FuryNull -> FVOID

let rec typeofexpression e = match e with
  |FuryLessThan (e1,e2) -> FBOOL
  |FuryMoreThan (e1,e2) -> FBOOL
  |FuryEqualTo (e1,e2) -> FBOOL
  |FuryPlus (e1,e2) -> FINT
  |FuryMinus (e1,e2) -> FINT
  |FuryTimes (e1,e2) -> FINT
  |FuryDivide (e1,e2) -> FINT
  |FuryNegate (e1) -> FINT
  |FuryIf (e1,e2,e3) -> FVOID
  |FuryFor (e1, e2, e3) -> FVOID
  |FuryRead -> FLIST
  |FuryWrite(n) -> FINT
  |FuryDeclare(t, n) -> FVOID
  |FuryDeclareAndBind(t, n, v) -> FVOID
  |FuryRebind(n, v) -> FVOID
  |_ -> FVOID
