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
    When using type alias to define a record type, or just creating a record
    like 'point', it creates access functions like .name (access the 'name'
    property of record), like below:

    > .name
    <function> : { b | name : a } -> a
    > .age
    <function> : { b | age : a } -> a

    They are the same as the below lambda function
    .name == \r -> r.name
-}
name = .name s1
name2 = .name { address = "SZ", age = 22.5, name = "zhangst" }



output : String
output =
    toString <| [name, name2, s2.name]
