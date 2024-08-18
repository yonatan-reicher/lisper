structure PARSE = struct

open AST

type tokens = string list;

fun tokenize x = 
  String.tokens (fn c: char => c = #" ") 
    (String.translate
       (fn #"(" => " ( "
         | #")" => " ) "
         | #"\n" => " "
         | #"'" => " ' "
         | c => str c)
       x)

exception BadList of SExp * tokens

fun quote term = CONS (ATOM (SYMBOL "quote"), CONS(term, ATOM NIL))

fun parse_list tokens: SExp * tokens =
    case parse_term tokens of
      NONE => (ATOM NIL, tokens)
    | SOME (left, rest) => 
        let val (right, rest) = parse_list rest in
          (CONS (left, right), rest)
        end

and parse_term ("(" :: ")" :: rest) = SOME (ATOM NIL, rest) : (SExp * tokens) option
  | parse_term ("(" :: rest) =
  (case parse_list rest
     of (expr, ")" :: rest) => SOME (expr, rest)
      | (expr, rest) => raise BadList (expr, rest))
  | parse_term (")" :: rest) = NONE
  | parse_term ("'" :: rest) =
    Option.map (fn (expr, rest) => (quote expr, rest)) (parse_term rest)
  | parse_term (symbol :: rest) = SOME (ATOM (SYMBOL symbol), rest)
  | parse_term [] = NONE;

exception EOFLeft of SExp * tokens
exception NotAnExpression of tokens

fun parse tokens = 
  (case parse_term tokens
     of SOME (parsed, []) => parsed
      | SOME (parsed, rest) => raise EOFLeft (parsed, rest)
      | NONE => raise NotAnExpression tokens)

end
