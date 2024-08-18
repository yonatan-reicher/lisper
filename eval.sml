structure EVAL = struct

open AST
open PARSE
open ENV

exception LispError;

local
    (* Helper functions - feel free to delete *)
    (* ====================================== *)
    fun is_digit c = c >= #"0" andalso c <= #"9";

    fun is_number str =
        let
            fun check [] = true
              | check (c::cs) = is_digit c andalso check cs
            
            val chars = String.explode str
        in
            if List.null chars then false else check chars
        end;
        
    fun char_to_int c = ord(c) - ord(#"0")

    fun string_to_int str =
        let
            fun convert [] acc = acc
            | convert (c::cs) acc = convert cs (10 * acc + char_to_int c)
        in
            convert (String.explode str) 0
        end;

    fun sexp_to_int sexp =
        case sexp of
            ATOM (SYMBOL s) => string_to_int s
          | _ => raise LispError;
    (* ====================================== *)

    fun zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)
      | zip _ = [];

    fun sexp_to_list (ATOM NIL) = SOME []
      | sexp_to_list (CONS(x, xs)) =
        Option.map 
          (fn list => x :: list)
          (sexp_to_list xs)
      | sexp_to_list _ = NONE

  val t: SExp = ATOM (SYMBOL "t");
  val n: SExp = ATOM NIL;

  fun car (CONS (x, _)) = x
    | car _ = raise LispError

  fun cdr (CONS (_, x)) = x
    | cdr _ = raise LispError

  fun cadr x = car (cdr x)

  fun cdar x = cdr (car x)

  fun caar x = car (car x)

  fun cddr x = cdr (cdr x)

  fun caddr x = car (cddr x)

  fun cadar x = car (cdr (car x))

  fun caddar x = car (cddr (car x))

  fun cons x y = CONS (x, y)

  fun atom (ATOM _) = t
    | atom _ = n

  fun append (ATOM NIL) a = a
    | append (CONS (x, a)) b = CONS(x, append a b)

  fun lst [] = ATOM NIL
    | lst (h::t) = CONS (h, lst t)

  fun toList (ATOM NIL) = []
    | toList (CONS (h, t)) = h :: toList t
    | toList (ATOM (SYMBOL s)) = raise LispError

  fun pair (ATOM NIL) (ATOM NIL) = ATOM NIL
    | pair (CONS (a1, a)) (CONS (b1, b)) =
    cons (lst [a1, b1]) (pair a b)

  fun assoc x env = ENV.find x env handle Undefined => raise LispError

  (* For some reason, only return true if both are atoms? *)
  fun eq (ATOM NIL) (ATOM NIL) = t
    | eq (ATOM (SYMBOL s1)) (ATOM (SYMBOL s2)) = if s1 = s2 then t else n
    | eq _ _ = n

    fun eval_built_in env "cons" [car_sexp, cdr_sexp] = let
          val car = eval' car_sexp env
          val cdr = eval' cdr_sexp env
        in
          SOME (CONS (car, cdr))
        end
      | eval_built_in env "car" [arg_sexp] =
        SOME (case eval' arg_sexp env of
                CONS (a, _) => a
              | _ => raise LispError)
      | eval_built_in env "cdr" [arg_sexp] =
        SOME (case eval' arg_sexp env of
                CONS (_, d) => d
              | _ => raise LispError)
      | eval_built_in env "quote" [arg_sexp] = SOME arg_sexp
      | eval_built_in env "atom" [arg_sexp] =
        SOME (case eval' arg_sexp env of
                ATOM _ => t
              | _ => ATOM NIL)
      | eval_built_in env "null" [arg_sexp] =
        SOME (case eval' arg_sexp env of
               ATOM NIL => t
             | _ => ATOM NIL)
      | eval_built_in env "eq" [arg1_sexp, arg2_sexp] =
        let val arg1 = eval' arg1_sexp env
            val arg2 = eval' arg2_sexp env
        in
          (* For some reason, only return true if both are atoms? *)
          SOME (case (arg1, arg2) of
                  (ATOM NIL, ATOM NIL) => t
                | (ATOM (SYMBOL s1), ATOM (SYMBOL s2)) => if s1 = s2 then t else ATOM NIL
                | _ => ATOM NIL)
        end
      | eval_built_in env "cond" [] = SOME (ATOM NIL)
      | eval_built_in env "cond" (clause::clauses) =
        (case sexp_to_list clause of
           SOME [test_sexp, result_sexp] =>
             (case eval' test_sexp env of
                ATOM NIL => eval_built_in env "cond" clauses
              | _ => SOME (eval' result_sexp env))
          | _ => raise LispError)
      | eval_built_in env "+" [a, b] = eval_int_op (op +) a b env
      | eval_built_in env "-" [a, b] = eval_int_op (op -) a b env
      | eval_built_in env "*" [a, b] = eval_int_op (op *) a b env
      | eval_built_in env "/" [a, b] = eval_int_op (op div) a b env
      | eval_built_in env "mod" [a, b] = eval_int_op (op mod) a b env
      | eval_built_in env "<" [a, b] = eval_comp_op (op <) a b env
      | eval_built_in env ">" [a, b] = eval_comp_op (op >) a b env
      | eval_built_in env "=" [a, b] = eval_comp_op (op =) a b env
      | eval_built_in env "/=" [a, b] = eval_comp_op (op <>) a b env
      | eval_built_in env "lambda" [params_sexp, body_sexp] =
        (* Do nothing! Another case handles actual evaluation *)
        SOME (CONS (ATOM (SYMBOL "lambda"), 
              CONS (params_sexp,
              CONS (body_sexp,
              ATOM NIL))))
      | eval_built_in env _ _ = NONE

    and eval_int_op f a b env = 
      let val a = eval' a env
          val b = eval' b env
          val res = f (sexp_to_int a, sexp_to_int b)
      in SOME (ATOM (SYMBOL (Int.toString res)))
      end

    and eval_comp_op f a b env =
      let val a = eval' a env
          val b = eval' b env
          val res = f (sexp_to_int a, sexp_to_int b)
      in SOME (if res then t else ATOM NIL)
      end

    and eval' (ATOM NIL) a = ATOM NIL
      | eval' (ATOM (SYMBOL "t")) _ = t
      | eval' (ATOM (SYMBOL s)) env =
      if is_number s then ATOM (SYMBOL s)
      else (find s env handle Undefined => raise LispError)
      | eval' (e as CONS (ATOM (SYMBOL s), _)) a =
      (case s
         of "quote" => cadr e
          | "atom" => atom (eval' (cadr e) a)
          | "eq" => eq (eval' (cadr e) a) (eval' (caddr e) a)
          | "car" => car (eval' (cadr e) a)
          | "cdr" => cdr (eval' (cadr e) a)
          | "cons" => cons (eval' (cadr e) a) (eval' (caddr e) a)
          | "cond" => evcon (cdr e) a
          | _ => eval' (cons (assoc s a) (cdr e)) a)
      | eval' (e as CONS (CONS (ATOM (SYMBOL "label"), _), _)) a =
      let val ATOM (SYMBOL sym_cadar) = cadar e
      in eval' (cons (caddar e) (cdr e)) (defineNested sym_cadar a (car e))
      end
      | eval' (e as CONS (CONS (ATOM (SYMBOL "lambda"),
                                CONS(params, CONS(body, ATOM NIL))),
                          args)) a =
      let val argsList = toList args
          val paramsList = toList params
          val paramNamesList = List.map (fn ATOM (SYMBOL s) => s | _ => raise
          LispError) paramsList
      in eval' body (pushEnv (toEnv (ListPair.zip (paramNamesList, evlis
      argsList a))) a)
      end
      | eval' sexp env = raise LispError

  and evcon c a =
    if eval' (caar c) a = t then eval' (cadar c) a
    else evcon (cdr c) a

  and evlis [] _ = []
    | evlis (h::t) a = (eval' h a)::(evlis t a)

  and toEnv [] = ENV.initEnv()
    | toEnv ((n, v)::t) = define n (toEnv t) v
          
in
    fun eval (string_exp: string) (env: envs): SExp * envs =
        let val exp = parse (tokenize string_exp) in
            (eval' exp env, env)
        end
        handle LispError => (ATOM (SYMBOL "lisp-error"), env)
end

end
