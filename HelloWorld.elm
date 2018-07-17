{-
    First command line program in Elm.

    We need to have JavaScript package 'run-elm' installed. If not,
    following this link to do it:

    https://github.com/jfairbank/run-elm

    To run the program on command line, do:

    $ run-elm HelloWorld.elm
    Hello World!
-}

-- module name must match the file name, first letter capitalized
module HelloWorld exposing(..)

{-
    The default output function must be named as 'output'.
    It must evalutes to type String.
-}
output : String
output =
    "Hello, World!"


{- This doesn't work
output : Int
output =
    3
-}
