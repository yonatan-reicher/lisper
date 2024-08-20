structure Parse = struct

open Ast

type tokens = string list;

fun consChar c = fn (s, r) => (Char.toString c ^ s, r)

fun lexString (#"\"" :: rest) = SOME ("", rest)
  | lexString [] = NONE
  | lexString (a :: rest) =
  Option.map (consChar a) (lexString rest)
  
fun lexIdent rest =
  case rest
    of ([] | (#" " | #"\n" | #"(" | #")" | #"'" | #"\"") :: _) => ("", rest)
     | c :: rest => consChar c (lexIdent rest)

fun nextToken (#"(" :: rest) = SOME ("(", rest)
  | nextToken (#")" :: rest) = SOME (")", rest)
  | nextToken ((#"\n" | #" ") :: rest) = nextToken rest
  | nextToken (#"'" :: rest) = SOME ("'", rest)
  | nextToken (#"\"" :: rest) = 
  Option.map (fn (s, r) => ("\"" ^ s ^ "\"", r)) (lexString rest)
  | nextToken [] = NONE
  | nextToken rest = SOME (lexIdent rest)
  
fun tokenizeList l: tokens =
  case nextToken l
    of SOME (t, r) => t :: tokenizeList r
     | NONE => []

fun tokenize x = 
  tokenizeList (String.explode x)

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
  | parse_term (symbol :: rest) =
  if List.exists (fn #"\"" => true | _ => false) (String.explode symbol)
  then SOME (quote (ATOM (SYMBOL (String.substring (symbol, 1, String.size
  symbol - 2)))), rest)
  else SOME (ATOM (SYMBOL symbol), rest)
  | parse_term [] = NONE;

exception EOFLeft of SExp * tokens
exception NotAnExpression of tokens

fun parse tokens = 
  (case parse_term tokens
     of SOME (parsed, []) => parsed
      | SOME (parsed, rest) => raise EOFLeft (parsed, rest)
      | NONE => raise NotAnExpression tokens)

end

