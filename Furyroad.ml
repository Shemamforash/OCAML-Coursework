exception TypeError ;;
exception Terminated ;;
exception UnboundVariableError;;
exception LookupError;;
exception StuckTerm;;
exception NonBaseTypeResult;;

open Functions

type furytype = FINT | FLIST | FBOOL | FSTRING | FURYVOID

type furyterm =
    FuryInt of int
  | FuryBool of bool
  | FuryList of int list
  | FuryString of string
  | FuryLessThan of furyterm * furyterm
  | FuryMoreThan of furyterm * furyterm
  | FuryEqualTo of furyterm * furyterm
  | FuryPlus of furyterm * furyterm
  | FuryMinus of furyterm * furyterm
  | FuryDivide of furyterm * furyterm
  | FuryTimes of furyterm * furyterm
  | FuryNegate of furyterm
  | FuryIf of furyterm * furyterm * furyterm
(*  | FuryFor of furyterm * furyterm * furyterm
  | FuryVar of string *)
  | FuryRead
  | FuryWrite of furyterm
  | FuryDeclare of furyterm * furyterm * furyterm

type 'a context = Env of (furytype * 'a) list
type typeContext = furytype context
type valContext = furyterm context

let bind env name newobject = match env with
      Env(tail) ->  Env( (name, newobject) :: tail ) ;;

let rec typeOf env e = match e with
   FuryInt (n) -> FINT
  |FuryBool (b) -> FBOOL
  |FuryList (l) -> FLIST
  |FuryString (s) -> FSTRING
  (*|FuryVar (x) ->  (try (lookupobject x env) with LookupError -> raise TypeError)*)
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
  |FuryDeclare(t, n, v) -> FURYVOID (*type name value*)

(*let rec lookupobject objectname evnironment = match environment with
    [] -> raise LookupError
  | ((binding,existingobject) :: gs) ->
        (
          match (binding = objectname) with
            true -> existingobject
           |false -> lookupobject ( (gs)) objectname
	)
;;*)

(*if e is a value eg. an int, bool, string, or list then return true since we don't need to evaluate it*)
let rec isValue e = match e with
  | FuryInt(n) -> true
  | FuryBool(b) -> true
  | FuryString(s) -> true
  | FuryList(l) -> true
  | _ -> false
;;

let rec evalnoenv env e = match e with
(* | (FuryVar x) -> (try (lookupobject x) with LookupError -> raise UnboundVariableError) *)
  | (FuryInt n) -> raise Terminated
  | (FuryBool b) -> raise Terminated
  | (FuryString s) -> raise Terminated
  | (FuryList l) -> raise Terminated

  | (FuryNegate (FuryInt(n))) -> (FuryInt (-n))
  | (FuryNegate (e1)) -> let e1' = (evalnoenv env e1) in (FuryNegate(e1'))

  | (FuryLessThan(FuryInt(n),FuryInt(m))) -> (FuryBool( n < m ))
  | (FuryLessThan(FuryInt(n), e2))      -> let e2' = (evalnoenv env e2) in (FuryLessThan(FuryInt(n),e2'))
  | (FuryLessThan(e1, e2))            -> let e1' = (evalnoenv env e1) in (FuryLessThan(e1',e2))

  | (FuryMoreThan(FuryInt(n),FuryInt(m))) -> (FuryBool( n > m ))
  | (FuryMoreThan(FuryInt(n), e2))      -> let e2' = (evalnoenv env e2) in (FuryMoreThan(FuryInt(n),e2'))
  | (FuryMoreThan(e1, e2))            -> let e1' = (evalnoenv env e1) in (FuryMoreThan(e1',e2))

  | (FuryEqualTo(FuryInt(n),FuryInt(m))) -> (FuryBool( n = m ))
  | (FuryEqualTo(FuryInt(n), e2))      -> let e2' = (evalnoenv env e2) in (FuryEqualTo(FuryInt(n),e2'))
  | (FuryEqualTo(e1, e2))            -> let e1' = (evalnoenv env e1) in (FuryEqualTo(e1',e2))

  | (FuryPlus(FuryInt(n),FuryInt(m))) -> (FuryInt( n + m ))
  | (FuryPlus(FuryInt(n), e2))      -> let e2' = (evalnoenv env e2) in (FuryPlus(FuryInt(n),e2'))
  | (FuryPlus(e1, e2))            -> let e1' = (evalnoenv env e1) in (FuryPlus(e1', e2))

  | (FuryMinus(FuryInt(n),FuryInt(m))) -> (FuryInt( n - m ))
  | (FuryMinus(FuryInt(n), e2))      -> let e2' = (evalnoenv env e2) in (FuryMinus(FuryInt(n),e2'))
  | (FuryMinus(e1, e2))            -> let e1' = (evalnoenv env  e1) in (FuryMinus(e1', e2))

  | (FuryDivide(FuryInt(n),FuryInt(m))) -> (FuryInt( n / m ))
  | (FuryDivide(FuryInt(n), e2))      -> let e2' = (evalnoenv env e2) in (FuryDivide(FuryInt(n),e2'))
  | (FuryDivide(e1, e2))            -> let e1' = (evalnoenv env e1) in (FuryDivide(e1', e2))

  | (FuryTimes(FuryInt(n),FuryInt(m))) -> (FuryInt( n * m ))
  | (FuryTimes(FuryInt(n), e2))      -> let e2' = (evalnoenv env e2) in (FuryTimes(FuryInt(n),e2'))
  | (FuryTimes(e1, e2))            -> let e1' = (evalnoenv env e1) in (FuryTimes(e1', e2))

  | (FuryIf(FuryBool(true),e1,e2))    -> e1
  | (FuryIf(FuryBool(false),e1,e2))   -> e2
  | (FuryIf(b,e1,e2))               -> let b' = (evalnoenv env b) in (FuryIf(b',e1,e2))

  | (FuryRead) -> (FuryList(Functions.read))

  | (FuryWrite(FuryInt(n))) -> (Functions.write(n)) ; raise Terminated
  | (FuryWrite(e1)) -> let e1' = (evalnoenv env e1) in (FuryWrite(e1'))

  | (FuryDeclare(t, FuryString(name), value)) -> let expr_type = typeOf value env in declare env name t value

  | _ -> raise Terminated ;;

let rec evalloop e = try ( let e' = evalnoenv env e in evalloop e') with Terminated -> if (isValue e) then e else raise StuckTerm ;;
let evalProg e = evalloop e ;;
let typeProg e = typeOf (Env []) e ;;

let print_res res = match res with
    | (FuryInt i) -> print_int i ; print_string " : Int"
    | (FuryBool b) -> print_string (if b then "true" else "false") ; print_string " : Bool"
    | _ -> raise NonBaseTypeResult
