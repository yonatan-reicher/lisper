structure PRETTY = struct
  fun pretty_atom Ast.NIL = "()"
    | pretty_atom (Ast.SYMBOL a) = a

  fun cons_to_list (Ast.ATOM NIL) = SOME []
    | cons_to_list (Ast.CONS (h, t)) =
    Option.map (fn l => h :: l) (cons_to_list t)

  fun pretty (Ast.ATOM a) = pretty_atom a
    | pretty (Ast.CONS (h, t)) = 
    (case cons_to_list (Ast.CONS (h, t))
       of SOME l => "(" ^ String.concatWith " " (List.map pretty l) ^ ")"
        | NONE => "(" ^ pretty h ^ " . " ^ pretty t ^ ")")
end
