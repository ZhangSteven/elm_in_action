{-
    Test how type 'List' works
-}
module TestList exposing(..)


{-
    The function List.member tells whether a value is in a list. Below
    is our own implementation for it.

    For a list, the below is equivalent:

    [1,2,3,4]
    1::[2,3,4]
    1::2::3::4::[]

    Here we use the :: operator to do the destructuring of a list, x and
    xs are just variable names representing the first element and the tail
    list. You can use other names if you want.

    Note that we use recursive calls to get the result.
-}
member : a -> List a -> Bool
member value list =
    case list of
        [] -> False
        (x::xs) ->
            if x == value then
                True
            else
                member value xs


{-
    Tell us some of the first elements

    Again, use :: to do pattern matching
-}
tell : List a -> String
tell list =
    case list of
        [] -> "The list is empty"
        (x::[]) -> "The list has one element: " ++ toString x
        (x::y::[]) -> "The list has two elements: " ++ toString x ++
            " and " ++ toString y
        (x::y::_) -> "The list is long. The first two elements are: " ++
            toString x ++ " and " ++ toString y


output : String
output =
    -- toString (member "haha" ["ok", "", "haha"])
    -- toString (member "hello" ["ok", "", "haha"])
    -- toString (member 5 [1, 5, 9])
    -- tell ['a', 'b', 'c']
    -- tell [2]
    tell ["hello", "world"]
