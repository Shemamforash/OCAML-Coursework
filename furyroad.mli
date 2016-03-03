type furytype = FINT | FLIST | FBOOL | FSTRING

type furyterm =
    FNum of int
  | FBool of bool
  | FList of list
  | FSTRING of string
  | FLessThan of furyterm * furyterm
  | FMoreThan of furyterm * furyterm
  | FEqualTo of furyterm * furyterm
  | FPlus of furyterm * furyterm
  | FMinus of furyterm * furyterm
  | FDivide of furyterm * furyterm
  | FTimes of furyterm * furyterm
  | FVar of string
  | FIf of furyterm * furyterm
  | FFor of furyterm * furyterm * furyterm
  | FRead of int
  | FWrite of int
