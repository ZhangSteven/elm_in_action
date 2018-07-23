{-
    Record in Elm
-}

-- module name must match the file name, first letter capitalized
module TestRecord exposing(..)


{-
    A record is a set of key value pairs, similar to Objects in
    JavaScript.
-}
point = { x = 0, y = 1 }    -- OK


{-
    If we want to use a record as a type, use type alias
-}
type alias Student = { name : String, age : Int }


{-
    type alias actually defines a function that can create records
    of the same type.

    > Student
    <function> : String -> Int -> Repl.Student
-}
s1 = Student "Chloe" 6  -- same as : s1 = { name = "Chloe", age = 6 }
s2 = Student "Andy" 5


{-
    When using type alias to define a record type, or just create a record
    like 'point', it creates

    See https://guide.elm-lang.org/core_language.html

    > .name
    <function> : { b | name : a } -> a
    > .age
    <function> : { b | age : a } -> a

-}



output : String
output =
    "Hello, World!"
