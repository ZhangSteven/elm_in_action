{-
    Test how type 'tuple' works
-}
module TestTuple exposing(..)


sumVector : (number, number, number) -> number
sumVector v =
    {-
        here use use (a, b, c) to declare the type, meaning any type.

        use (x, _, _) in tuple destruction, meaning we don't care about
        the those elements
    -}
    let
        first : (a, b, c) -> a
        first (x, _, _) = x

        second : (a, b, c) -> b
        second (_, y, _) = y

        third : (a, b, c) -> c
        third (_, _, z) = z
    in
        (first v + second v + third v)


output : String
output =
    toString (sumVector (1, 2.5, 3))
