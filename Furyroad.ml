exception TypeError ;;
exception Terminated ;;

type furytype = FINT | FLIST | FBOOL | FSTRING

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
  | FuryVar of string
  | FuryIf of furyterm * furyterm * furyterm
  | FuryFor of furyterm * furyterm * furyterm
  | FuryRead of int
  | FuryWrite of int

let rec typeOf  e = match e with
   FuryInt (n) -> FINT
  |FuryBool (b) -> FBOOL
  |FuryList (l) -> FLIST
  |FuryString (s) -> FSTRING

  |FuryVar (x) ->  (try lookup  x with LookupError -> raise TypeError)

  |FuryLessThan (e1,e2) ->
      ( match (typeOf  e1) , (typeOf  e2) with
          FINT, FINT -> FBOOL
        | _ -> raise TypeError
      )
  |FuryMoreThan (e1,e2) ->
      ( match (typeOf  e1) , (typeOf  e2) with
          FINT, FINT -> FBOOL
        | _ -> raise TypeError
      )
  |FuryEqualTo (e1,e2) ->
      ( match (typeOf  e1) , (typeOf  e2) with
          FINT, FINT -> FBOOL
        | _ -> raise TypeError
      )
  |FuryPlus (e1,e2) ->
      ( match (typeOf  e1) , (typeOf  e2) with
          FINT, FINT -> FINT
        | _ -> raise TypeError
      )
  |FuryMinus (e1,e2) ->
      ( match (typeOf  e1) , (typeOf  e2) with
          FINT, FINT -> FINT
        | _ -> raise TypeError
      )
  |FuryTimes (e1,e2) ->
      ( match (typeOf  e1) , (typeOf  e2) with
          FINT, FINT -> FINT
        | _ -> raise TypeError
      )
  |FuryDivide (e1,e2) ->
      ( match (typeOf  e1) , (typeOf  e2) with
          FINT, FINT -> FINT
        | _ -> raise TypeError
      )
  |FuryIf (e1,e2,e3) -> (
    let ty1 = typeOf  e1 in
      match ty1 with
         FBOOL -> (let ty1 = typeOf  e2 in
		                  let ty2 = typeOf  e3 in
		                    (match (ty1=ty2) with
		                      true -> ty1
		                      |false -> raise TypeError
		                    )
	                )
        |_ -> raise TypeError
    );;

let bind ironment name newobject = match  with
      (gs) ->  ( (name, newobject) :: gs ) ;;

let rec lookupobject objectname = match ironment with
    [] -> raise LookupError
  | ((binding,existingobject) :: gs) ->
        (
          match (binding = objectname) with
            true -> existingobject
           |false -> lookupobject ( (gs)) objectname
	)
;;

let rec evalnoenv e = match e with
  | (FuryVar x) -> (try (lookupobject x) with LookupError -> raise UnboundVariableError)
  | (FuryInt n) -> raise Terminated
  | (FuryBool b) -> raise Terminated
  | (FuryString s) -> raise Terminated
  | (FuryList l) -> raise Terminated

  | (FuryLessThan(FuryInt(n),FuryInt(m))) -> (FuryBool( n < m ))
  | (FuryLessThan(FuryInt(n), e2))      -> let e2' = (evalnoenv e2) in (FuryLessThan(FuryInt(n),e2'))
  | (FuryLessThan(e1, e2))            -> let e1' = (evalnoenv e1) in (FuryLessThan(e1',e2))

  | (FuryMoreThan(FuryInt(n),FuryInt(m))) -> (FuryBool( n > m ))
  | (FuryMoreThan(FuryInt(n), e2))      -> let e2' = (evalnoenv e2) in (FuryMoreThan(FuryInt(n),e2'))
  | (FuryMoreThan(e1, e2))            -> let e1' = (evalnoenv e1) in (FuryMoreThan(e1',e2))

  | (FuryEqualTo(FuryInt(n),FuryInt(m))) -> (FuryBool( n = m ))
  | (FuryEqualTo(FuryInt(n), e2))      -> let e2' = (evalnoenv e2) in (FuryEqualTo(FuryInt(n),e2'))
  | (FuryEqualTo(e1, e2))            -> let e1' = (evalnoenv e1) in (FuryEqualTo(e1',e2))

  | (FuryPlus(TmNum(n),TmNum(m))) -> (FuryInt( n + m ))
  | (FuryPlus(TmNum(n), e2))      -> let e2' = (evalnoenv e2) in (FuryPlus(FuryInt(n),e2'))
  | (FuryPlus(e1, e2))            -> let e1' = (evalnoenv e1) in (FuryPlus(e1', e2))

  | (FuryMinus(TmNum(n),TmNum(m))) -> (FuryInt( n - m ))
  | (FuryMinus(TmNum(n), e2))      -> let e2' = (evalnoenv e2) in (FuryMinus(FuryInt(n),e2'))
  | (FuryMinus(e1, e2))            -> let e1' = (evalnoenv  e1) in (FuryMinus(e1', e2))

  | (FuryDivide(TmNum(n),TmNum(m))) -> (FuryInt( n / m ))
  | (FuryDivide(TmNum(n), e2))      -> let e2' = (evalnoenv e2) in (FuryDivide(FuryInt(n),e2'))
  | (FuryDivide(e1, e2))            -> let e1' = (evalnoenv e1) in (FuryDivide(e1', e2))

  | (FuryTimes(TmNum(n),TmNum(m))) -> (FuryInt( n * m ))
  | (FuryTimes(TmNum(n), e2))      -> let e2' = (evalnoenv e2) in (FuryTimes(FuryInt(n),e2'))
  | (FuryTimes(e1, e2))            -> let e1' = (evalnoenv e1) in (FuryTimes(e1', e2))

  | (FuryIf(FuryBool(true),e1,e2))    -> e1
  | (FuryIf(FuryBool(false),e1,e2))   -> e2
  | (FuryIf(b,e1,e2))               -> let b' = (evalnoenv b) in (FuryIf(b',e1,e2))

  | _ -> raise Terminated ;;
