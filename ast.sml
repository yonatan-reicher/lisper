structure AST = struct

datatype Atom =
    SYMBOL of string
  | NIL

datatype SExp = 
    ATOM of Atom
  | CONS of SExp * SExp

end
