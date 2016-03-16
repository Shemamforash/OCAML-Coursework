open Functions;;
open Types;;
open EvaluationFunctions;;
open Exceptions;;
open Printer;;

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
  | (FuryVar x) ->  print_res e ; let (n, (t, v)) = lookup x env in v

  | (FuryPrimitive p) -> evaluateprimitive p

  | (FuryNegate (e1)) -> let e1' = (evaluate env e1) in if (isexprtype env e1' FINT) then negate e1' else raise TypeMismatch

  | (FuryLessThan(e1, e2)) -> print_res e ; (applymathsfunc lessthan env e1 e2)

  | (FuryMoreThan(e1, e2)) ->  print_res e ; (applymathsfunc morethan env e1 e2)

  | (FuryEqualTo(e1, e2)) ->  print_res e ; (applymathsfunc equalto env e1 e2)

  | (FuryPlus(e1, e2)) ->  print_res e ; (applymathsfunc plus env e1 e2)

  | (FuryMinus(e1, e2)) ->  print_res e ; (applymathsfunc minus env e1 e2)

  | (FuryDivide(e1, e2)) ->  print_res e ; (applymathsfunc divide env e1 e2)

  | (FuryTimes(e1, e2)) ->  print_res e ; (applymathsfunc times env e1 e2)

  | (FuryIf(b,e1,e2)) -> print_res e ; let b' = (evaluate env b) in if ((isexprtype env b' FBOOL)) then (match b' with
                                      | FuryBool(true) -> let env2 = Environment(env, Hashtbl.create(5)) in evaluateSequence env2 e1
                                      | FuryBool(false) -> let env2 = Environment(env, Hashtbl.create(5)) in evaluateSequence env2 e2
                                      | _ -> raise TypeMismatch)
                                      else raise TypeMismatch

  | (FuryFor(e1, e2, e3)) -> print_res e ; let env2 = Environment (env, (Hashtbl.create(5))) in (evaluate env2 e1) ;
                                  (if (isexprtype env2 (evaluate env2 e2) FBOOL) then (
                                      while (let b = (evaluate env2 e2) in match b with FuryBool b -> b | _ -> raise TypeError) do
                                         let env3 = Environment(env2, (Hashtbl.create(5))) in (evaluateSequence env3 e3)
                                      done ; FuryNull)
                                  else raise TypeMismatch)

  | (FuryDeclare(t, name)) -> print_res e ; (try (let (_, _) = lookup name env in raise VariableAlreadyDeclared)
                                         with RootEnvironmentLeft -> (match t with
                                        | FLIST -> (bind env name (t, FuryList([])))
                                        | FINT -> (bind env name (t, FuryNull))
                                        | _ -> raise TypeError) ; FuryNull)

  | (FuryDeclareAndBind(t, name, value)) -> print_res e ; let value' = evaluate env value in if t=(typeofprimitive value') then
                            (try (let (_, _) = (lookup name env) in raise VariableAlreadyDeclared) with RootEnvironmentLeft -> (bind env name (t, value')) ; FuryNull)
                              else raise TypeMismatch

  | (FuryRebind(name, value)) -> print_res e ; let (env2, (t, value2)) = lookup name env in
                                    let value' = evaluate env value in
                                      if (((typeofprimitive value')=t)||(t=FVOID))
                                        then ((bind env2 name (t, value')) ; FuryNull) else raise TypeMismatch

  | (FuryWrite(e1)) -> print_res e ; (match evaluate env e1 with
                                      | FuryList(n) -> write n
                                      | FuryInt(n) -> print_int n ; print_string "\n"
                                      | _ -> raise TypeMismatch) ; FuryNull

  | (FuryAddToList(name, value)) -> print_res e ; let (env2, (t, list2)) = lookup name env in
                                                    let value' = evaluate env value in
                                                      (match value' with
                                                        | FuryInt(n) -> (match list2 with
                                                                          | FuryList(ls) -> (bind env2 name (t, (FuryList(n::ls))))
                                                                          | _ -> raise TypeMismatch)
                                                        | _ -> raise TypeMismatch) ; FuryNull

  | (FuryGetFromList(name, pos)) -> print_res e ; let (env2, (t, list2)) = lookup name env in
                                                    let pos' = evaluate env pos in
                                                      (match pos' with
                                                        | FuryInt(n) -> (match list2 with
                                                                          | FuryList(ls) -> if (List.length ls) > n then (FuryInt(List.nth ls ((List.length ls) - n - 1))) else raise OutOfBounds
                                                                          | _ -> raise TypeMismatch)
                                                        | _ -> raise TypeMismatch)

  | (FuryIsListEmpty(name)) -> print_res e ; let (env2, (t, list2)) = lookup name env in (match list2 with
                                                        | FuryList(ls) -> if (List.length ls)=0 then FuryBool(false) else FuryBool(true)
                                                        | _ -> raise TypeMismatch )

  | (FuryRead) -> print_res e ; FuryList(readstream ())

  | _ -> raise TypeMismatch

and applymathsfunc func env e1 e2 = let e1' = (evaluate env e1) in
                  let e2' = (evaluate env e2) in
                    if ((isexprtype env e1' FINT) || (isexprtype env e1' FFLOAT)) then
                        (if ((isexprtype env e2' FINT) || (isexprtype env e2' FFLOAT)) then (func e1' e2') else raise TypeMismatch)
                          else raise TypeMismatch

and evaluateSequence env seq = match seq with
  | ([])                -> FuryNull
  | (e1 :: [])          -> evaluate env e1
  | (e1 :: e2)          -> evaluate env e1 ; evaluateSequence env e2
