open Exceptions;;
open Types;;

let rec lookup objectname environment = match environment with
    | NullEnvironment -> raise RootEnvironmentLeft
    | Environment(parent, lst) -> try (environment, Hashtbl.find lst objectname) with Not_found -> (lookup objectname parent)

let bind env name newobject = match env with
    | Environment(_, hash) -> Hashtbl.replace hash name newobject
    | NullEnvironment -> raise RootEnvironmentLeft

let negate n = match n with
  | (FuryInt n) -> FuryInt(-n)
  | _ -> raise TypeError

let lessthan i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryBool(i1 < i2)
  | (FuryFloat i1), (FuryFloat i2) -> FuryBool(i1 < i2)
  | (FuryInt i1), (FuryFloat i2) -> FuryBool((float_of_int i1) < i2)
  | (FuryFloat i1), (FuryInt i2) -> FuryBool(i1 < (float_of_int i2))
  | _ -> raise TypeError

let morethan i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryBool(i1 > i2)
  | (FuryFloat i1), (FuryFloat i2) -> FuryBool(i1 > i2)
  | (FuryInt i1), (FuryFloat i2) -> FuryBool((float_of_int i1) > i2)
  | (FuryFloat i1), (FuryInt i2) -> FuryBool(i1 > (float_of_int i2))
  | _ -> raise TypeError

let equalto i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryBool(i1 = i2)
  | (FuryFloat i1), (FuryFloat i2) -> FuryBool(i1 = i2)
  | (FuryInt i1), (FuryFloat i2) -> FuryBool((float_of_int i1) = i2)
  | (FuryFloat i1), (FuryInt i2) -> FuryBool(i1 = (float_of_int i2))
  | _ -> raise TypeError

let plus i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryInt(i1 + i2)
  | (FuryFloat i1), (FuryFloat i2) -> FuryFloat(i1 +. i2)
  | (FuryInt i1), (FuryFloat i2) -> FuryFloat((float_of_int i1) +. i2)
  | (FuryFloat i1), (FuryInt i2) -> FuryFloat(i1 +. (float_of_int i2))
  | _ -> raise TypeError

let minus i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryInt(i1 - i2)
  | (FuryFloat i1), (FuryFloat i2) -> FuryFloat(i1 -. i2)
  | (FuryInt i1), (FuryFloat i2) -> FuryFloat((float_of_int i1) -. i2)
  | (FuryFloat i1), (FuryInt i2) -> FuryFloat(i1 -. (float_of_int i2))
  | _ -> raise TypeError

let divide i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryFloat((float_of_int i1) /. (float_of_int i2))
  | (FuryFloat i1), (FuryFloat i2) -> FuryFloat(i1 /. i2)
  | (FuryInt i1), (FuryFloat i2) -> FuryFloat((float_of_int i1) /. i2)
  | (FuryFloat i1), (FuryInt i2) -> FuryFloat(i1 /. (float_of_int i2))
  | _ -> raise TypeError

let times i1 i2 = match i1, i2 with
  | (FuryInt i1), (FuryInt i2) -> FuryInt(i1 * i2)
  | (FuryFloat i1), (FuryFloat i2) -> FuryFloat(i1 *. i2)
  | (FuryInt i1), (FuryFloat i2) -> FuryFloat((float_of_int i1) *. i2)
  | (FuryFloat i1), (FuryInt i2) -> FuryFloat(i1 *. (float_of_int i2))
  | _ -> raise TypeError
