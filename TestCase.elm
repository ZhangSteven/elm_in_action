{-
    Test how 'case' expression works
-}
module TestCase exposing(..)


{-
    When there are multiple branches, we can use a 'case' expression

    Note case expressions must have the same identation, like:

    'a' -> xxx
    _   -> xxx

    If they are not aligned, like:

    'a' -> xxx
      _ -> xxx

    Then error will occur
-}
charName : Char -> String
charName c =
    case c of
        'a' -> "Albert"
        'b' -> "Broseph"
        'c' -> "Cecil"
        _   -> "Some Others"    -- The default case, use '_' means we don't
                                -- care what value is passed


change : Int -> Int
change n =
    case n of
        1 -> 10
        2 -> 100
        x -> x * 10     -- Use x to represent all other values


output : String
output =
    charName 'd' ++ " " ++ toString (change 9)
