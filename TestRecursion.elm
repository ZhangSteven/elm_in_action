{-
    Test how recursion works
-}
module TestRecursion exposing(..)


{-
    Find out the the maximum of a list

    This works but not as elegant as the below one.

maximum : List comparable -> comparable
maximum list =
    case list of
        [] -> Debug.crash "maximum of empty list"
        [x] -> x    -- same as using (x::[])
        (x::xs) ->
            let
                maxTail = maximum xs
            in
                if x > maxTail then
                    x
                else
                    maxTail
--}


maximum : List comparable -> comparable
maximum list =
    case list of
        [] -> Debug.crash "maximum of empty list"
        [x] -> x    -- same as using (x::[])
        (x::xs) -> max x (maximum xs)


-- repeat the element n times
repeat : Int -> a -> List a
repeat n el =
    if n <= 0 then
        []
    else
        el :: (repeat (n-1) el)


-- take the first n elements from a list
take : Int -> List a -> List a
take n list =
    if n <= 0 then
        []
    else
        case list of
            [] -> []
            (x::xs) -> x :: (take (n-1) xs)


-- reverse a list
reverse : List a -> List a
reverse list =
    case list of
        [] -> []
        (x::xs) -> reverse xs ++ [x]



-- zip two lists together
{- Not as elegant as the below solution
zip : List a -> List b -> List (a, b)
zip lista listb =
    if lista == [] || listb == [] then
        []
    else
        (List.head lista, List.head listb) ::
            (zip (List.tail lista) (List.tail listb))
-}

zip : List a -> List b -> List (a, b)
zip lista listb =
    case (lista, listb) of
        ([], _) -> []
        (_, []) -> []
        ((x::xs), (y::ys)) -> (x, y) :: (zip xs ys)


{-
    A more elegant implementation of quicksort.

    Note the '>' operator (other operators '+', '/', etc. also) is a
    function normally used in 'infix' style, like:

        a + b

    it's the same as:

        (+) a b

    Therefore, the below is the same:

        ((>) x) y

        x > y

    So when we want to filter out those smaller than x, we use ((>) x)
    as the filter function.
-}
quicksort : List comparable -> List comparable
quicksort list =
    case list of
        [] -> []
        (x::xs) ->
            let
                smallerSorted = quicksort (List.filter ((>) x) xs)
                largerSorted =  quicksort (List.filter ((<=) x) xs)
            in
                smallerSorted ++ [x] ++ largerSorted


output : String
output =
    -- toString (maximum [1, 88, 102, 99])
    -- toString (maximum [])
    -- toString (maximum ["hello"])
    -- toString (repeat 5 "hello")
    -- toString (take 2 [1, 2, 88])
    -- toString (take 3 ["ok", "good"])
    -- toString (take 2 [])
    -- toString (reverse [1, 2, 3])
    -- toString (zip [1, 2] ["hello", "ok", "good"])
    toString (quicksort [1, 5, 3, 11, 5, 8])
