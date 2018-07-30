{-
    Test how Dict works
-}
module TestDict exposing (..)
import Dict exposing (Dict)





output : String
output =
    -- toString <| Dict.insert "x" 8 Dict.empty

    {-
        Update a dictionary value using a function.

        If the key exists, update its value using the function, othereise
        do nothing.
    -}
    -- let
    --     list = [("x", 8), ("y", 3.55)]
    --     d = Dict.fromList list
    --
    --     addOne : Maybe number -> Maybe number
    --     addOne =
    --         Maybe.map <| (+) 1
    --
    -- in
    --     toString <| Dict.update "y" addOne d
        -- toString <| Dict.update "z" addOne d    -- key doesn't exist


    {-
        Update a dictionary.

        If the key exists, modify its value. Othereise create a new value.
    -}
    let
        list = [("x", [88]), ("y", [3.55])]
        d = Dict.fromList list

        append : Maybe (List number) -> Maybe (List number)
        append x =
            case x of
                Nothing -> Just [n]
                Just xs -> Just (n::xs)

        n = 90

    in
        -- toString <| Dict.update "x" append d
        toString <| Dict.update "z" append d
