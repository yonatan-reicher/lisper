structure Main = struct
  fun evalString (expr: string) : string =
    PRETTY.pretty (#1 (Eval.eval expr (Env.emptyNestedEnv())))

  fun readFile filename = TextIO.inputAll (TextIO.openIn filename)

  fun doRepl(): unit =
    (print "> ";
     case TextIO.inputLine TextIO.stdIn
       of NONE => ()
        | SOME "\n" => doRepl()
        | SOME input =>
          let val prettyOutput = evalString input
          in print (prettyOutput ^ "\n");
             doRepl()
          end)


  fun evalFile (filename: string): string =
    evalString (readFile filename)

  val charToSExp = Ast.ATOM o Ast.SYMBOL o Char.toString
  val stringToSExp = Ast.ofList o List.map charToSExp o String.explode
  fun quote e = Ast.CONS(Ast.ATOM (Ast.SYMBOL "quote"), Ast.CONS(e, Ast.ATOM Ast.NIL)) 

  fun evalFileOn (filename: string) (input: string): string =
    let val code = readFile filename
        val (result, _) = Eval.eval code (Env.emptyNestedEnv())
        val input = quote (stringToSExp input)
        val toRun = Ast.CONS(result, Ast.CONS(input, Ast.ATOM Ast.NIL))
        val result = Eval.eval' toRun (Env.emptyNestedEnv())
    in PRETTY.pretty result
    end

  val main : (string * string list) -> OS.Process.status =
    fn (name, args) => (doRepl(); OS.Process.success)
end

