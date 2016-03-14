exception TypeError ;;
exception Terminated ;;
exception UnboundVariableError;;
exception LookupError;;
exception StuckTerm;;
exception RootEnvironmentLeft;;
exception TypeMismatch;;

open Functions;;
open Types;;
open EvaluationFunctions;;

let rec lookup objectname environment = match environment with
    | NullEnvironment -> print_string ("No binding for " ^ objectname) ; raise RootEnvironmentLeft
    | Environment(parent, lst) -> try (environment, Hashtbl.find lst objectname) with Not_found -> (lookup objectname parent)

let bind env name newobject = match env with
    | Environment(_, lst) ->  Hashtbl.replace lst name newobject
    | NullEnvironment -> raise RootEnvironmentLeft

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
  |FuryDeclare(t, n, v) -> FVOID
  |FuryRebind(n, v) -> FVOID
  |FuryListDeclare(n) -> FVOID
  |_ -> FVOID

(*if e is a value eg. an int, bool, string, or list then return true since we don't need to evaluate it*)
let rec isValue e = match e with
  | FuryInt(n) -> true
  | FuryBool(b) -> true
  | FuryString(s) -> true
  | FuryList(l) -> true
  | _ -> false
;;

let prin n v = print_string n;;
let rec printEnv e = match e with
  | Environment (p, h) -> Hashtbl.iter prin h ; printEnv p
  | NullEnvironment -> print_string ""

let evaluateprimitive p = match p with
  | (FuryInt n) -> FuryInt(n)
  | (FuryBool b) -> FuryBool(b)
  | (FuryString s) -> FuryString(s)
  | (FuryList l) -> FuryList(l)
  | (FuryVoid v) -> FuryVoid(v)
  | (FuryFloat f) -> FuryFloat(f)
  | (FuryNull) -> FuryNull

let isexprtype env e ty = (typeofprimitive e)=ty

let rec evaluate (env:environment) e = match e with
  | (FuryVar x) ->  print_res e ; let (n, v) = (try (lookup x env) with LookupError -> raise UnboundVariableError) in v
  | (FuryPrimitive p) ->  evaluateprimitive p

  | (FuryNegate (e1)) -> let e1' = (evaluate env e1) in if (isexprtype env e1' FINT) then negate e1' else raise TypeMismatch

  | (FuryLessThan(e1, e2)) -> print_res e ; (applymathsfunc lessthan env e1 e2)

  | (FuryMoreThan(e1, e2)) ->  print_res e ; (applymathsfunc morethan env e1 e2)

  | (FuryEqualTo(e1, e2)) ->  print_res e ; (applymathsfunc equalto env e1 e2)

  | (FuryPlus(e1, e2)) ->  print_res e ; (applymathsfunc plus env e1 e2)

  | (FuryMinus(e1, e2)) ->  print_res e ; (applymathsfunc minus env e1 e2)

  | (FuryDivide(e1, e2)) ->  print_res e ; (applymathsfunc divide env e1 e2)

  | (FuryTimes(e1, e2)) ->  print_res e ; (applymathsfunc times env e1 e2)

  | (FuryIf(b,e1,e2)) -> print_res e ; let b' = (evaluate env b) in if ((isexprtype env b' FBOOL)) then match b' with
                                      | FuryBool(true) -> let env2 = Environment(env, Hashtbl.create(5)) in evaluateSequence env2 e1
                                      | FuryBool(false) -> let env2 = Environment(env, Hashtbl.create(5)) in evaluateSequence env2 e2
                                      | _ -> raise TypeMismatch
                                    else raise TypeMismatch

  | (FuryFor(e1, e2, e3)) -> print_res e ; let env2 = Environment (env, (Hashtbl.create(5))) in
                                  let v = (evaluate env2 e1) in
                                  if (isexprtype env2 (evaluate env2 e2) FBOOL) then (
                                      while (let b = (evaluate env2 e2) in match b with FuryBool b -> b | _ -> raise TypeError) do
                                         let env3 = Environment(env2, (Hashtbl.create(5))) in evaluateSequence env3 e3
                                      done ; FuryNull )
                                  else raise TypeMismatch

  | (FuryDeclare(t, name, value)) -> print_res e ; if t=FINT then let name' = evaluate env name in (match name' with
                                        | FuryString(n) -> ((bind env n (evaluate env value)) ; FuryNull)
                                        | _ -> FuryNull) else raise TypeError

  | (FuryRebind(name, value)) -> print_res e ; let (env2, value2) = lookup name env in
                                    let value' = evaluate env value in
                                      if ((typeofprimitive value')=(typeofprimitive value2))
                                        then ((bind env2 name value') ; FuryNull) else raise TypeMismatch

  | (FuryWrite(e1)) -> print_res e ; let e1' = (evaluate env e1) in (match e1' with
                                            | FuryInt(n) -> write n ; FuryNull
                                            | _ -> raise TypeMismatch)

  | (FuryListDeclare(name)) -> print_res e ; (bind env name (FuryList([]))) ; FuryNull

  | (FuryAddToList(name, value)) -> print_res e ; let (env2, list2) = lookup name env in
                                                    let value' = evaluate env value in
                                                      (match value' with
                                                        | FuryInt(n) -> (match list2 with
                                                                          | FuryList(ls) -> (bind env name (FuryList(n::ls)))
                                                                          | _ -> raise TypeMismatch)
                                                        | _ -> raise TypeMismatch) ; FuryNull

  (*
  | (FuryRead) -> (FuryList(Functions.read))
  *)

  | _ -> raise Terminated

and applymathsfunc func env e1 e2 = let e1' = (evaluate env e1) in
                  let e2' = (evaluate env e2) in
                    if ((isexprtype env e1' FINT) && (isexprtype env e2' FINT)) then (func e1' e2') else raise TypeMismatch

and evaluateSequence env seq = match seq with
  | ([])                -> FuryNull
  | (e1 :: [])          -> evaluate env e1
  | (e1 :: e2)          -> evaluate env e1 ; evaluateSequence env e2
