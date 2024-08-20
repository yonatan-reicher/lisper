# Lisper 

A LISP interpreter adapted from HW4 and HW3

- [Usage](#usage)
  - [Repl Main Functions](#repl-main-functions)
- [Example Files](#example-files)

## Usage

First, you must have SMLNJ installed on your machine. Then you run the
following
command:
```sh
sml sources.cm main.sml
```

Now you are in an SML repl, and the functions in Main should be displayed
infront of you. Run the following command to start a LISP repl:
```sml
Main.doRepl();
```

### Repl Main Functions

* `Main.doRepl();` - Starts a LISP repl.
* `Main.evalString <program>;` - Takes a LISP program as a string and returns
                                 it's result.
* `Main.evalFile <filename>` - Runs the program in the given file and returns
                               it's result.
* `Main.evalFileOn <filename> <input>`
  - Takes the program at the given file, and runs it on the given input.
    The input is a string that will be given to the program as a list of one-
    character atoms.
    The program must be return function. That function will be applied the
    input.

## Example Files

* `hello.lisp` - Just a simple hello world.
                 Run with `Main.evalFile "hello.lisp"`.
* `rev.lisp` - A function that reverses a string.
               Run with `Main.evalFileOn "rev.lisp" "abcd";`.
* `parse.lisp` - An S-Expression parser written in lisp.
                 Run with `Main.evalFileOn "parse.lisp" "(cons 'a 'b)";`.
* `eval.lisp` - An S-Expression parser written in lisp.
                Run with `Main.evalFile "eval.lisp"`.
                To change the input, modify the third line.
