exception TypeError ;;
exception Terminated ;;
exception UnboundVariableError;;
exception LookupError;;
exception StuckTerm;;
exception NonBaseTypeResult;;
exception RootEnvironmentLeft;;

open Functions;;
open Types;;

let rec lookup objectname environment = match environment with
    | NullEnvironment -> raise RootEnvironmentLeft
    | Environment(parent, lst) -> (try (environment, Hashtbl.find lst objectname) with Not_found -> lookup objectname parent)

let bind env name newobject = match env with
    | Environment(_, lst) ->  Hashtbl.replace lst name newobject
    | NullEnvironment -> raise RootEnvironmentLeft

let rec typeOf env e = match e with
   FuryInt (n) -> FINT
  |FuryBool (b) -> FBOOL
  |FuryList (l) -> FLIST
  |FuryString (s) -> FSTRING
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
  |FuryNegate (e1) -> FINT
  |FuryIf (e1,e2,e3) -> (
    let ty1 = typeOf env e1 in
      match ty1 with
         FBOOL -> (let ty1 = typeOf env e2 in
		                  let ty2 = typeOf env e3 in
		                    (match (ty1=ty2) with
		                      true -> ty1
		                      |false -> raise TypeError
		                    )
	                )
        |_ -> raise TypeError
    )
  |FuryRead -> FLIST
  |FuryWrite(n) -> FINT
  |FuryDeclare(t, n, v) -> FVOID (*type name value*)
  |FuryVoid(u) -> FVOID

(*if e is a value eg. an int, bool, string, or list then return true since we don't need to evaluate it*)
let rec isValue e = match e with
  | FuryInt(n) -> true
  | FuryBool(b) -> true
  | FuryString(s) -> true
  | FuryList(l) -> true
  | _ -> false
;;

let rec evaluate (env:environment) e = match e with
  | (FuryVar x) -> let (n, v) = (try (lookup x env) with LookupError -> raise UnboundVariableError) in v
  | (FuryInt n) -> FuryInt(n)
  | (FuryBool b) -> FuryBool(b)
  | (FuryString s) -> FuryString(s)
  | (FuryList l) -> FuryList(l)
  | (FuryVoid v) -> FuryVoid(v)

  | (FuryNegate (FuryInt(n))) -> FuryInt (-n)
  | (FuryNegate (e1)) -> let e1' = (evaluate env e1) in FuryNegate(e1')

  | (FuryLessThan(FuryInt(n),FuryInt(m))) -> FuryBool( n < m )
  | (FuryLessThan(FuryInt(n), e2))      -> let e2' = (evaluate env e2) in FuryLessThan(FuryInt(n),e2')
  | (FuryLessThan(e1, e2))            -> let e1' = (evaluate env e1) in FuryLessThan(e1',e2)

  | (FuryMoreThan(FuryInt(n),FuryInt(m))) -> FuryBool( n > m )
  | (FuryMoreThan(FuryInt(n), e2))      -> let e2' = (evaluate env e2) in FuryMoreThan(FuryInt(n),e2')
  | (FuryMoreThan(e1, e2))            -> let e1' = (evaluate env e1) in FuryMoreThan(e1',e2)

  | (FuryEqualTo(FuryInt(n),FuryInt(m))) -> FuryBool( n = m )
  | (FuryEqualTo(FuryInt(n), e2))      -> let e2' = (evaluate env e2) in FuryEqualTo(FuryInt(n),e2')
  | (FuryEqualTo(e1, e2))            -> let e1' = (evaluate env e1) in FuryEqualTo(e1',e2)

  | (FuryPlus(FuryInt(n),FuryInt(m))) -> FuryInt( n + m )
  | (FuryPlus(FuryInt(n), e2))      -> let e2' = (evaluate env e2) in FuryPlus(FuryInt(n),e2')
  | (FuryPlus(e1, e2))            -> let e1' = (evaluate env e1) in FuryPlus(e1', e2)

  | (FuryMinus(FuryInt(n),FuryInt(m))) -> FuryInt( n - m )
  | (FuryMinus(FuryInt(n), e2))      -> let e2' = (evaluate env e2) in FuryMinus(FuryInt(n),e2')
  | (FuryMinus(e1, e2))            -> let e1' = (evaluate env  e1) in FuryMinus(e1', e2)

  | (FuryDivide(FuryInt(n),FuryInt(m))) -> FuryInt( n / m )
  | (FuryDivide(FuryInt(n), e2))      -> let e2' = (evaluate env e2) in FuryDivide(FuryInt(n),e2')
  | (FuryDivide(e1, e2))            -> let e1' = (evaluate env e1) in FuryDivide(e1', e2)

  | (FuryTimes(FuryInt(n),FuryInt(m))) -> FuryInt( n * m )
  | (FuryTimes(FuryInt(n), e2))      -> let e2' = (evaluate env e2) in FuryTimes(FuryInt(n),e2')
  | (FuryTimes(e1, e2))            -> let e1' = (evaluate env e1) in FuryTimes(e1', e2)

  | (FuryIf(FuryBool(true),e1,e2))    -> e1
  | (FuryIf(FuryBool(false),e1,e2))   -> e2
  | (FuryIf(b,e1,e2))               -> let b' = (evaluate env b) in FuryIf(b',e1,e2)

  | (FuryRead) -> (FuryList(Functions.read))

  | (FuryWrite(FuryInt(n))) -> Functions.write(n) ; raise Terminated
  | (FuryWrite(e1)) -> let e1' = (evaluate env e1) in FuryWrite(e1')

  | (FuryDeclare(t, FuryString(name), value)) -> FuryVoid(bind env name value);

  | _ -> raise Terminated ;;

(* let rec evalloop env e = try (let (e',env') = (evaluate env e) in (evalloop env' e')) with Terminated -> if (isValue e) then e else raise StuckTerm  ;;
let evalProg e = evalloop (Env []) e ;;
let typeProg e = typeOf (Env []) e ;; *)
