structure Env = struct

open Ast

exception Undefined;
exception Empty;

type env = (string -> SExp);
type envs = env list;

(*fun initEnv () = fn (str:string):SExp => raise Undefined;*)
fun initEnv (): env = fn str => if false then ATOM( SYMBOL str) else raise Undefined;

fun define (str:string) f v = fn s => if str = s then v else f(s);

fun emptyNestedEnv (): envs = [initEnv()];

(*is it better to use hd and tl?*)
fun pushEnv a l: envs = a::l;
fun popEnv (x :: sx) = sx
    |popEnv [] = raise Empty;

fun topEnv (x :: sx) = x
    |topEnv [] = raise Empty;

fun defineNested str lst v = let
  val head = topEnv(lst)
in
  (define str head v)::(popEnv lst)
end;


fun find (str:string) [] : SExp = raise Undefined
  | find str (env :: envs) = 
    env str handle Undefined => find str envs

end
