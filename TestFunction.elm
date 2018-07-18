{-
    Functions in Elm
-}
module TestFunction exposing(..)


{-
    In Elm, everything function officially takes only one parameter. So
    how is it possible that we defined and used functions that take more
    than one parameter so far?

    Because all the functions that accepted several parameters are curried
    functions. For example, the max function which takes two parameters and
    returns the one that's bigger. Doing max 4 5 first creates a function
    that takes a parameter and returns either 4 or that parameter, depending
    on which is bigger. Then 5 is applied to that function and that function
    produces our desired result. Therefore,

    max 4 5 is actually (max 4) 5
-}

multiThree x y z =
    x * y * z

multiTwoWith9 =
    multiThree 9

multiWith18 =
    multiTwoWith9 2


{-
    Higher order function

    Functions can take functions as parameters and also return functions.

    Below function takes another function as input and apply it twice on
    something.
-}
applyTwice : (a -> a) -> a -> a
applyTwice f x = f (f x)


square : number -> number
square x =
    x * x


-- Apply square twice
square2 = applyTwice square


{-
    Find the largest number under 100,000 that's divisible by 3829.

    Note we use 'Maybe' keyword here, it's because in the case that
    no value is divisible by 3829 in the range of [1, 100000), then
    'Nothing' will be returned.
-}
largestDivisible : Int -> Int -> Maybe Int
largestDivisible divider n =
    let
        isDivisible divider n =
            n % divider == 0
    in
        List.maximum <| List.filter (isDivisible divider) <| List.range 1 n


{-
    Find the sum of all odd squares that are smaller than 10,000.

    To solve this problem, we do:
    1. produce square over a list of odds, but how to define such a list?
    2. do sum over the squared list, add it up until we hit a square larger
        than 10,000, then we stop.

    To do (2), we first create a function called 'takeWhile'. It takes a
    predicate and a list and then goes from the beginning of the list and
    returns its elements while the predicate holds true.
-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile p list =
    case list of
        [] -> []
        (x::xs) ->
            if p x then
                x :: takeWhile p xs
            else
                []  -- stop recursion here


{-
    Collatz sequence. For a natual number n, do the following to produce
    the next number n1,

        if n is even, n1 = n / 2
        else n1 = 3*n + 1

    Keeping doing it and eventually it will become 1 (角谷猜想).

    Now, what we want to know is this: for all starting numbers between
    1 and 100, how many chains have a length greater than 15?

    [1..100] -- map to chain -- [chain1..chain100] -- filter length > 15
    -- [chain..chain] -- count array length -- result

    Therefore we need to do is to create a function to produce a sequence
    that ends at 1.
-}
chain : Int -> List Int
chain n =
    let
        isEven : Int -> Bool
        isEven n =
            n % 2 == 0
    in
        case n of
            1 -> [1]
            n ->
                if isEven n then
                    n :: chain (n // 2)
                else
                    n :: chain (3*n + 1)


{-
    fold a list: It's Elm's 'reduce' function, but here it's called fold.

    Whenever we traverse a list once, element by element, then produce
    some value, chances are we are going to need a fold. So it's a very
    useful function.

    There are two versions of fold, List.foldl (start from left) or
    List.foldr (start from right), called as:

        List.foldl f initialValue list

    Previously we have used recursion to implement 'sum' and 'map', we
    can do it with fold, much more elegant.
-}

-- sum all the elements in a list
sum : List number -> number
-- This is OK.
-- sum list =
--     List.foldl (+) 0 list
--
-- But since 'list' is on both sides of the equation, we can take it
-- out and write the function as:
sum =
    List.foldl (+) 0


{-
    We can also implement map using foldl, as below.

    Using foldr or foldl both achieve the result, however the foldr
    implementation is considered better, because in Elm, the :: operation
    is much cheaper than the ++ operator. So if we are building a List
    from a list, then foldr is usually a better option.

map f list =
    List.foldl (\x acc -> acc ++ [(f x)]) [] list
---}
map : (a -> b) -> List a -> List b
map f list =
    List.foldr (\x acc -> (f x) :: acc) [] list


-- find maximum of a list
maximum : List comparable -> Maybe comparable
-- unfortunately this does not compile
-- maximum =
--     let
--         bigger a b =
--             if b == Nothing || b < a then
--                 a
--             else
--                 b
--     in
--         List.foldl bigger Nothing -- this is a function

-- the solution from the book
maximum =
    let
        bigger x acc =
            if
                case acc of
                    Nothing -> True
                    Just n -> x > n
            then
                Just x
            else
                acc
    in
        List.foldl bigger Nothing -- this is a function



output : String
output =
    -- toString [
    --     toString multiThree
    --     , toString multiTwoWith9
    --     , toString multiWith18
    --     , toString (multiThree    9  2  10)
    --     , toString (multiTwoWith9 2  10)
    --     , toString (multiWith18   10)
    -- ]

    -- toString [
    --     square2 8
    --     , applyTwice square 8
    -- ]

    -- toString <| largestDivisible 3829 100000

    -- toString <| takeWhile (flip (<) 10) <| List.range 1 100

    -- sum of all odd squares smaller than 10,000
    -- let
    --     isOdd : Int -> Bool
    --     isOdd n =
    --         n % 2 /= 0
    -- in
    --     toString <| List.sum <| takeWhile (flip (<) 10000) <| List.map square
    --         <| List.filter isOdd <| List.range 1 9999

    -- toString <| chain 30

    -- 角谷猜想
    -- let
    --     longEnough : List a -> Bool
    --     longEnough list =
    --         if List.length list > 15 then
    --             True
    --         else
    --             False
    -- in
    --     toString <| List.length <| List.filter longEnough
    --         <| List.map chain <| List.range 1 100

    -- Test fold functions
    -- toString <| sum <| List.range 1 100
    -- toString <| map ((+) 3) <| List.range 1 5
    -- toString <| maximum []
    toString <| maximum [1, 3, 11, 8]
