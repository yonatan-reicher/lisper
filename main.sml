structure MAIN = struct
  fun evalString (expr: string) : string =
    PRETTY.pretty (#1 (EVAL.eval expr (ENV.emptyNestedEnv())))

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


  fun evalFile (filename: string): unit =
    print (evalString (readFile filename) ^ "\n")
    

  val main : (string * string list) -> OS.Process.status =
    fn (name, args) => (doRepl(); OS.Process.success)
end

