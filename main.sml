structure MAIN = struct
  fun doRepl(): unit =
    (print "> ";
     case TextIO.inputLine TextIO.stdIn
       of NONE => ()
        | SOME "\n" => doRepl()
        | SOME input =>
          let val (output, _) = EVAL.eval input (ENV.emptyNestedEnv ())
              val pretty_output = PRETTY.pretty output
          in print (pretty_output ^ "\n");
             doRepl()
          end)
    

  val main : (string * string list) -> OS.Process.status =
    fn (name, args) => (doRepl(); OS.Process.success)
end

