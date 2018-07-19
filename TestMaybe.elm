{-
    Maybe: a container for value, like List. The difference is: Maybe
    contains at most one value.

    list : List String
    Defines list as a List of String values, can be empty.

    a : Maybe String
    Defines a as either a String value or Nothing.
-}
module TestMaybe exposing(..)


{-
    Maybe <type> =
        Just <type> |
        Nothing

    Where 'Nothing' is a predefined value in Elm, and 'Just' is a function
    which takes a value and returns a Maybe object wrapping that value.

    To assign a value to a binding of type Maybe <type>, use Nothing or
    call the function 'Just' with the value.
-}
a : Maybe Int
a = Nothing

b : Maybe Int
-- b = 10  type mismatch, use 'Just' function to create Maybe Int type
b = Just 10


{-
    Compare Maybe values to others.

    Compares variable 'x' of some value to a Maybe value 'm', and returns
    the bigger one. The rules are:

    1. If the Maybe value is Nothing, then return x
    2. If the Maybe value contains something, then do comparison and returns
    the bigger one.

    Here is a version that returns a value
-}
bigger : comparable -> Maybe comparable -> comparable
bigger x m =
    case m of
        Nothing -> x
        {-
            'Just value' is the way to destructure a Maybe value, in
            this case, 'value' represents the value inside the Maybe
            container. Using other variable name like 'Juse n' does
            the same.
        -}
        Just value ->
            if x > value then
                x
            else
                value


{-
    A slight different way to implement the bigger function. But this one
    returns a 'Maybe' value. This can be useful in situations like a
    List maxixum algorightm, where the bigger function serves as the
    accumulator.
-}
bigger2 : comparable -> Maybe comparable -> Maybe comparable
bigger2 x m =
    if
        case m of
            Nothing -> True
            Just value -> x > value
    then
        Just x  -- need to use 'Just x' instead of 'x', because the return
                -- type is 'Maybe'
    else
        m


{-
    Max of maximums.

    Suppose we have a list of lists of numbers, like below:

    [ [1, 2.5, 3], [], [88] ]

    We need to find out the ultimate maximum of all the numbers in there.

    Here we got some useful tools:

    List.filterMap : (a -> Maybe b) -> List a -> List b

    It takes a function which maps a to Maybe b, then filters out those
    Nothing values, and for the remaining, take out their inner values to
    form a new list.

    For example, if we want a function which takes in List (Maybe a) and
    gives us List a, filtering out those Noting values, we can define it
    as:

    innerValues = List.filterMap identity
-}
maximum : List (List comparable) -> Maybe comparable
maximum list =
    let
        -- Take a list of Maybe values, filter out those are Nothing
        -- and put the inner values of the rest Maybes into a new list
        innerValues : List (Maybe a) -> List a
        innerValues =
            List.filterMap identity

        listOfMax : List (Maybe comparable)
        listOfMax =
            List.map List.maximum list
    in
        List.maximum <| innerValues listOfMax


-- a more compact implementation, same as maximum
maximum2 : List (List comparable) -> Maybe comparable
maximum2 =
    List.maximum << (List.filterMap List.maximum)



output : String
output =
    -- toString [a, b]
    -- toString <| bigger 10 Nothing    -- 10
    -- toString <| bigger 10 <| Just 12    -- 12

    -- Error, type mismatch
    -- toString <| bigger 10 <| 12

    -- toString <| bigger2 10 <| Nothing

    let
        {-
            Maybe.map : (a -> b) -> Maybe a -> Maybe b
            It takes a mapping function (a -> b), apply it on
            a value (Maybe a), then:
                if it is Nothing: return Nothing
                if it is Just a : apply (a -> b), returns Just b

            This function is useful in situations where you want to
            change a Maybe value, but is not at an appropriate place
            to handle the case of Nothing. So you just leave Nothing
            as is, waiting for functions up the chain who are at a
            better position to handle it.

            Maybe.withDefault : a -> Maybe a -> a
            It takes a default value and a Maybe value, if the Maybe
            value is Nothing, then use the default value, otherwise
            use the Maybe's inner value.

            This function is used in situations where you know how to
            handle the case of Nothing.

            Combining the two functions, we can do something interesting,
            such as showing a Maybe value to the output.
        -}
        resultAsString : Maybe a -> String
        resultAsString =
            Maybe.withDefault "nothing" << Maybe.map toString
    in
        -- resultAsString <| maximum <| []
        -- resultAsString <| maximum <| [ [] ]
        -- resultAsString <| maximum <| [ [1, 8.8, 3], [], [12.5], [-2, 11] ]

        -- resultAsString <| maximum2 <| []
        -- resultAsString <| maximum2 <| [ [] ]
        resultAsString <| maximum2 <| [ [1, 8.8, 3], [], [12.5], [-2, 11] ]
