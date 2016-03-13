exception TypeError;;

open Types;;

let negate n = match n with
  | (FuryInt n) -> FuryInt(-n)
  | _ -> raise TypeError

let lessthan i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryBool(i1 < i2)
  | _ -> raise TypeError

let morethan i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryBool(i1 > i2)
  | _ -> raise TypeError

let equalto i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryBool(i1 = i2)
  | _ -> raise TypeError

let plus i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryInt(i1 + i2)
  | _ -> raise TypeError

let minus i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryInt(i1 - i2)
  | _ -> raise TypeError

let divide i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryFloat((float_of_int i1) /. (float_of_int i2))
  | _ -> raise TypeError

let times i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryInt(i1 * i2)
  | _ -> raise TypeError
