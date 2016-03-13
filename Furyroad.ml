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

let rec typeOf env e = match e with
  |FuryPrimitive (p) -> typeofprimitive p
  |FuryVar (x) ->  let (l, o) = (try (lookup x env) with RootEnvironmentLeft -> raise Not_found) in
      ( match (typeOf env o) with
        | FINT -> FINT
        | FBOOL -> FBOOL
        | FLIST -> FLIST
        | FSTRING -> FSTRING
        | _ -> raise TypeError)
  |FuryLessThan (e1,e2) ->
      ( match (typeOf env e1) , (typeOf env e2) with
          FINT, FINT -> FBOOL
        | _ -> raise TypeError)
  |FuryMoreThan (e1,e2) ->
      ( match (typeOf env e1) , (typeOf env e2) with
          FINT, FINT -> FBOOL
        | _ -> raise TypeError)
  |FuryEqualTo (e1,e2) ->
      ( match (typeOf env e1) , (typeOf env e2) with
          FINT, FINT -> FBOOL
        | _ -> raise TypeError)
  |FuryPlus (e1,e2) ->
      ( match (typeOf env e1) , (typeOf env e2) with
          FINT, FINT -> FINT
        | _ -> raise TypeError)
  |FuryMinus (e1,e2) ->
      ( match (typeOf env e1) , (typeOf env e2) with
          FINT, FINT -> FINT
        | _ -> raise TypeError)
  |FuryTimes (e1,e2) ->
      ( match (typeOf env e1) , (typeOf env e2) with
          FINT, FINT -> FINT
        | _ -> raise TypeError)
  |FuryDivide (e1,e2) ->
      ( match (typeOf env e1) , (typeOf env e2) with
          FINT, FINT -> FINT
        | _ -> raise TypeError)
  |FuryNegate (e1) -> typeOf env e1
  |FuryIf (e1,e2,e3) -> (
    let ty1 = typeOf env e1 in
      match ty1 with
         FBOOL -> FVOID
        |_ -> raise TypeError
    )
  |FuryFor (e1, e2, e3) -> FVOID
  |FuryRead -> FLIST
  |FuryWrite(n) -> FINT
  |FuryDeclare(t, n, v) -> let ty = typeOf env v in (match (t=ty) with true -> FVOID | false -> raise TypeError) (*type name value*)
  |FuryRebind(n, v) -> let (e2, v2) = (lookup n env) in (match ((typeOf env v)=(typeOf e2 v2)) with true -> FVOID | false -> raise TypeError)

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

let isexprtype env e ty = if (typeofprimitive e)=ty then true else false;;

let rec evaluate (env:environment) e = match e with
  | (FuryVar x) -> let (n, v) = (try (lookup x env) with LookupError -> raise UnboundVariableError) in (evaluate env v)
  | (FuryPrimitive p) -> evaluateprimitive p

  | (FuryNegate (e1)) -> let e1' = (evaluate env e1) in if (isexprtype env e1' FINT) then negate e1' else raise TypeMismatch

  | (FuryLessThan(e1, e2)) -> (applymathsfunc lessthan env e1 e2)

  | (FuryMoreThan(e1, e2)) -> (applymathsfunc morethan env e1 e2)

  | (FuryEqualTo(e1, e2)) -> (applymathsfunc equalto env e1 e2)

  | (FuryPlus(e1, e2)) -> (applymathsfunc plus env e1 e2)

  | (FuryMinus(e1, e2)) -> (applymathsfunc minus env e1 e2)

  | (FuryDivide(e1, e2)) -> (applymathsfunc divide env e1 e2)

  | (FuryTimes(e1, e2)) -> (applymathsfunc times env e1 e2)

  | (FuryIf(b,e1,e2)) -> let b' = (evaluate env b) in if ((isexprtype env b' FBOOL)) then match b' with
                                      | FuryBool(true) -> let env2 = Environment(env, Hashtbl.create(5)) in evaluateSequence env2 e1
                                      | FuryBool(false) -> let env2 = Environment(env, Hashtbl.create(5)) in evaluateSequence env2 e2
                                      | _ -> raise TypeMismatch
                                    else raise TypeMismatch

  (*| (FuryIf(FuryPrimitive(FuryBool (true)),e1,e2))    -> let env2 = Environment(env, Hashtbl.create(5)) in evaluateSequence env2 e1
  | (FuryIf(FuryPrimitive(FuryBool(false)),e1,e2))   -> let env2 = Environment(env, Hashtbl.create(5)) in evaluateSequence env2 e2
  | (FuryIf(b,e1,e2))               -> let b' = (evaluate env b) in
                                        if (typeofprimitive b')=FBOOL then FuryVoid(FuryIf(b',e1,e2)) else raise TypeMismatch

  | (FuryFor(e1, e2, e3)) -> let env2 = Environment (env, (Hashtbl.create(5))) in
                                  let v = (evaluate env2 e1) in
                                  if (typeOf env2 (evaluate env2 e2))=FBOOL then (
                                      while not (let b = (evaluate env2 (evaluate env2 e2)) in match b with FuryBool b -> b | _ -> raise TypeError) do
                                         let env3 = Environment(env2, (Hashtbl.create(5))) in evaluateSequence env3 e3
                                      done ; FuryVoid() )
                                  else raise TypeMismatch

  | (FuryRead) -> (FuryList(Functions.read))

  | (FuryWrite(FuryInt(n))) -> Functions.write(n) ; raise Terminated
  | (FuryWrite(e1)) -> let e1' = (evaluate env e1) in FuryWrite(e1')

  | (FuryDeclare(t, FuryPrimitive(FuryString(name)), value)) -> if ((typeOf env e)=FVOID) then FuryVoid(bind env name (evaluate env value)) else raise TypeMismatch

  | (FuryRebind(name, FuryPrimitive(value))) -> if ((typeOf env e)=FVOID) then FuryVoid(assign env name value) else raise TypeMismatch
  | (FuryRebind(name, e1)) -> let e1' = (evaluate env e1) in FuryRebind(name, e1')*)

  | _ -> raise Terminated

and applymathsfunc func env e1 e2 = let e1' = (evaluate env e1) in
                  let e2' = (evaluate env e2) in
                    if ((isexprtype env e1' FINT) && (isexprtype env e2' FINT)) then (func e1' e2') else raise TypeMismatch

and evaluateSequence env seq = match seq with
  | ([])                -> FuryNull
  | (e1 :: [])          -> evaluate env e1
  | (e1 :: e2)          -> evaluate env e1 ; evaluateSequence env e2

(*and assign env name expr =
    try (
      let (env2, var) = lookup name env in
        let vartyp = typeOf env var in
          let vartyp2 = typeOf env expr in
            if vartyp=vartyp2 then bind env2 name (evaluate env expr) else raise TypeMismatch
      ) with Not_found -> raise Not_found*)

(* let rec evalloop env e = try (let (e',env') = (evaluate env e) in (evalloop env' e')) with Terminated -> if (isValue e) then e else raise StuckTerm  ;;
let evalProg e = evalloop (Env []) e ;;
let typeProg e = typeOf (Env []) e ;;*)
