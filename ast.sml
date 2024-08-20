structure Ast = struct

datatype Atom =
    SYMBOL of string
  | NIL

datatype SExp = 
    ATOM of Atom
  | CONS of SExp * SExp

exception NotAList of SExp

fun toList (ATOM NIL) = []
  | toList (CONS (h, t)) =
  ((h :: toList h)
   handle NotAList _ => raise NotAList (CONS (h, t)))
  | toList (ATOM (SYMBOL s)) = raise NotAList (ATOM (SYMBOL s))

fun ofList [] = ATOM NIL
  | ofList (h::t) = CONS(h, ofList t)

end
