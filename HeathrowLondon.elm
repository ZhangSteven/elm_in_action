{-
    The path finding problem
-}

-- module name must match the file name, first letter capitalized
module HeathrowLondon exposing(..)


{-
    The road system:

    --(50)--A1--(5)---A2--(40)--A3--(10)--A4
            |         |         |
            30        20        25        0
            |         |         |
    --(10)--B1--(90)--B2--(2)---B3--(8)---B4

    Find the shortest path to A4 and B4, the shorter of the two will
    be optimal path.
-}
type alias Section = { getA : Int, getB : Int, getC : Int }
type alias RoadSystem = List Section


-- Road system from Heathrow to London
heathrowToLondon : RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0]


type Label = A | B | C
type alias Path = List (Label, Int)

{-
    To find the best path to A4 and B4, we start by finding the best
    path to A1 and B1, from there we find best path to A2 and B2, and
    so on.

    roadStep finds the best path to A and B in a section given the best
    path up to the previosu section.
-}
roadStep : Section -> (Path, Path) -> (Path, Path)
roadStep { getA, getB, getC } (pathA, pathB) =
    let
        priceA = List.sum <| List.map Tuple.second pathA
        priceB = List.sum <| List.map Tuple.second pathB
        forwardPriceToA = priceA + getA
        crossPriceToA = priceB + getB + getC
        forwardPriceToB = priceB + getB
        crossPriceToB = priceA + getA + getC

        newPathA =
            if forwardPriceToA > crossPriceToA then
                (C, getC) :: (B, getB) :: pathB
            else
                (A, getA) :: pathA

        newPathB =
            if forwardPriceToB > crossPriceToB then
                (C, getC) :: (A, getA) :: pathA
            else
                (B, getB) :: pathB

    in
        (newPathA, newPathB)


optimalPath : RoadSystem -> Path
optimalPath roadSystem =
    let
        (pathA, pathB) = List.foldl roadStep ([],[]) roadSystem
        priceA = List.sum <| List.map Tuple.second pathA
        priceB = List.sum <| List.map Tuple.second pathB

        result =
            if priceA < priceB then
                pathA
            else if priceA > priceB then
                pathB
            else    -- priceA == priceB
                {-
                    In the case where cross cost between A4 and B4 is zero,
                    then pathA and pathB will always have a the same cost.
                    Therefore we choose the one that doesn't have the cross
                    over in the last step.
                -}
                case (List.head pathA) of
                    Just (C, 0) -> pathB
                    _ -> pathA

    in
        List.reverse result


{-
    Now let's move one step further. Convert an input string to a RoadSystem
    data structure, then find out its optimal path.

    The input string is a multi line string which looks like:

    input = """50
    10
    30
    5
    90
    20
    40
    2
    25
    10
    8
    0
    """

    First let's convert that string into a list of 3 strings, like
    [ ["50", "10", "30"], ["5", "90", "20"], ... ]
-}
groupsOf : Int -> List a -> Maybe (List (List a))
groupsOf n list =
    if n <= 0 then
        Nothing
    -- else if list == [] then
    --     Just []
    -- else if List.length list < n then
    --     Just [list]
    -- else
    --     Maybe.map (\rest -> (List.take n list) :: rest)
    --         <| groupsOf n <| List.drop n list

    else
        case (n, list) of
            (_, []) -> Just []
            (n, xs) ->
                Maybe.map (\rest -> List.take n xs :: rest)
                    <| groupsOf n (List.drop n xs)



output : String
output =
    -- toString <| roadStep { getA = 50, getB = 10, getC = 30 } ([], [])
    -- toString <| Maybe.map (flip roadStep ([],[])) <| List.head heathrowToLondon

    -- toString <| List.foldl roadStep ([],[]) heathrowToLondon
    -- toString <| optimalPath heathrowToLondon

    -- Test groupsOf
    -- toString <| groupsOf -2 [1, 2, 3]
    toString <| groupsOf 2 [1, 2, 3]
    -- toString <| groupsOf 3 [1, 2, 3]
    -- toString <| groupsOf 5 [1, 2, 3]
