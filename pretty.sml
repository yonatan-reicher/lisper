structure PRETTY = struct
  fun pretty_atom AST.NIL = "()"
    | pretty_atom (AST.SYMBOL a) = a

  fun cons_to_list (AST.ATOM NIL) = SOME []
    | cons_to_list (AST.CONS (h, t)) =
    Option.map (fn l => h :: l) (cons_to_list t)

  fun pretty (AST.ATOM a) = pretty_atom a
    | pretty (AST.CONS (h, t)) = 
    (case cons_to_list (AST.CONS (h, t))
       of SOME l => "(" ^ String.concatWith " " (List.map pretty l) ^ ")"
        | NONE => "(" ^ pretty h ^ " . " ^ pretty t ^ ")")
end
