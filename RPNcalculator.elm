{-
    Implement a RPN calculator

    This exercise comes from <Learn You an Elm>, chapter "Functionally
    Solve Problems".

    https://learnyouanelm.github.io/pages/10-functionally-solving-problems.html
-}
module RPNcalculator exposing(..)
import List exposing (head, foldl)
import String exposing (words)


{-
    Given an input string, like "10 4 3 + 2 * -", use the RPN way to calculate
    the result.

    Here is how we do calculation:

    1. Break the input string into a list of strings, e.g., "1 2 +" into
        ["1", "2", "+"]. This can be accomplished by String.words function.

    2. Traverse the list, for each element,
        if it is a number, push it to the top of the stack,
        if it is an operator, take the necessary numbers from the top of the
            stack, perform the operation, use the result and the remaining
            of the stack to create a new stack.

    3. When the whole list is finished, return the top of the stack.

    Question: What if an error occurs during the process, for example
    there aren't enough number of numbers needed by an operator, or something
    goes wrong in the calculation (such as 1/0)?

    In an imperative language, we can throw an exception to stop processing
    and let functions up the call chain to handle the problem. But what to
    do in a functional approach?

    The answer is to use a Maybe type to wrap the calculation result, when
    things go wrong, use Nothing to represent the result. Since the stack
    contains both numbers from the input and calculation results, then those
    numbers must be of Maybe types as well.
-}

solveRPN : String -> Maybe Float
solveRPN input =
    let
        flatten : Maybe (Maybe a) -> Maybe a
        flatten maybe =
            case maybe of
                Just x -> x
                Nothing -> Nothing

        accumulator : String -> List (Maybe Float) -> List (Maybe Float)
        accumulator x acc =
            case (x, acc) of
                ("+", x::y::ys) -> (Maybe.map2 (\a b -> a + b) x y) :: ys
                ("-", x::y::ys) -> (Maybe.map2 (\a b -> b - a) x y) :: ys
                ("*", x::y::ys) -> (Maybe.map2 (\a b -> a * b) x y) :: ys
                ("/", x::y::ys) -> (Maybe.map2 (\a b -> b / a) x y) :: ys
                ("ln", x::ys) -> (Maybe.map (\a -> logBase e a) x) :: ys
                ("sum", ys) -> [ foldl (Maybe.map2 (+)) (Just 0) ys ]

                {-
                    When it falls to this category, it's because:
                    1. It's a number, or
                    2. It's an unregnized operator, or
                    3. It's a valid operator but the stack has insufficient
                        number of elements.
                -}
                (value, xs) -> (Result.toMaybe <| String.toFloat value) :: xs

    in
        flatten <| head <| foldl accumulator [] <| words input


{-
    Implement solveRPN again, with the following modification:

    During the processing, if an error already occurred, i.e., the head
    of stack is Nothing or Infinity already in the stack, then the stack
    will stay unchanged.
-}
solveRPN2 : String -> Maybe Float
solveRPN2 input =
    let
        flatten : Maybe (Maybe a) -> Maybe a
        flatten maybe =
            case maybe of
                Just x -> x
                Nothing -> Nothing

        isNothingOrInfinity : Maybe Float -> Bool
        isNothingOrInfinity x =
            case x of
                Nothing -> True
                Just value -> isInfinite value

        accumulator : String -> List (Maybe Float) -> List (Maybe Float)
        accumulator x acc =
            {-
                If top of the stack is Nothing or Infinity, then stop processing
                new inputs, leave the stack unchanged.
            -}
            if (acc /= []) && (isNothingOrInfinity <| flatten <| head acc) then
                acc
            else
                case (x, acc) of
                    ("+", x::y::ys) -> (Maybe.map2 (\a b -> a + b) x y) :: ys
                    ("-", x::y::ys) -> (Maybe.map2 (\a b -> b - a) x y) :: ys
                    ("*", x::y::ys) -> (Maybe.map2 (\a b -> a * b) x y) :: ys
                    ("/", x::y::ys) -> (Maybe.map2 (\a b -> b / a) x y) :: ys
                    (value, xs) -> (Result.toMaybe <| String.toFloat value) :: xs

    in
        flatten <| head <| foldl accumulator [] <| words input



output : String
output =
    -- toString <| solveRPN ""
    -- toString <| solveRPN "3 4 +"
    -- toString <| solveRPN "10 3 4 + 2 * -"
    -- toString <| solveRPN "9 3 10 2 5 6 + * - + -"   -- Just 18
    -- toString <| solveRPN "9 3 + 10 2 5 sum 6 + ln 2 *"   -- Just 7.11

    {-
        Not enough number to add
    -}
    -- toString <| solveRPN "3 + 10 2 -"    -- Just 8
    -- toString <| solveRPN2 "3 + 10 2 -"   -- Nothing

    {-
        Infinity due to divided by zero.
    -}
    toString <| solveRPN2 "2 5 - 0 / 2 3 +"  -- Just -Infinity
    -- toString <| solveRPN "2 5 - 0 / 2 3 +"   -- Just 5
