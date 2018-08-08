{-
    "Our world is full of uncertainty. This uncertainty bleeds into our programs.
    A common way of dealing with this is null/nil. Unfortunately, this leads to
    even more uncertainty because this design means any value in our system
    could be null unless we've explicitly checked its presence. Constantly
    checking the presence of every value is a lot of work so we tend to check
    only the riskiest places and then have to deal with runtime null exceptions
    in the rest of our code."

    "The Mechanics of Maybe": from
    https://robots.thoughtbot.com/maybe-mechanics

    Elm and many other languages use a different approach to dealing with
    uncertainty: Maybe.

    In Elm, all values are guaranteed to be present except for those wrapped in
    a Maybe. This is a critical distinction. You can now be confident in most
    of your code and the compiler will force you to make presence-checks in
    places where values are optional.

    Its type definition looks like:

    type Maybe a =
        Just a
        | Nothing
-}
module TestMaybe exposing(..)
import Set exposing (Set)


{-
    The most basic way of dealing with a Maybe is via a case statement.
-}
type alias Book =
    { name : String }

type alias User =
    { name : String
    , age : Int
    , books : List Book }

firstUserName : List User -> Maybe String
firstUserName list =
    case List.head list of
        Nothing -> Nothing
        Just user -> Just user.name


{-
    Maybe.map and Maybe.andThen

    However, the above approach can quickly devolve into a nightmare of nested
    case statements. For example, we need to extract the name of the first user,
    then return an upper case version of the book name.

    -- This implementation works but not so elegant
    firstUserBook list =
        case List.head list of
            Nothing -> Nothing
            Just user ->
                case List.head user.books of
                    Nothing -> Nothing
                    Just book -> Just (String.toUpper book.name)

    Use helper functions. Suppose we want to do a transformation to a Maybe
    value, say Maybe a -> Maybe b. Then,

    1. If our function is (a -> b), use Maybe.map.
    2. If our function is (a -> Maybe b), use Maybe.andThen
-}
firstUserBook : List User -> Maybe String
firstUserBook list =
    Maybe.map (String.toUpper << .name)
        <| Maybe.andThen List.head
        <| Maybe.map .books
        <| List.head list


{-
    List.filterMap

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
    let
        isaac = User "Isaac" 6 [{ name = "mummies" }, { name = "knights" }]
        andy = User "Andy" 5 [{ name = "knights" }, { name = "stars" }]
        chloe = User "Chloe" 7 []
    in
        -- Maybe.withDefault "no users exist"
        --     <| firstUserName [isaac, andy, chloe]
        -- Maybe.withDefault "no users exist"
        --     <| firstUserName []
        Maybe.withDefault "no users or books exist"
            <| firstUserBook [isaac, andy, chloe]
        -- Maybe.withDefault "no users or books exist"
        --     <| firstUserBook []
        -- Maybe.withDefault "no users or books exist"
        --     <| firstUserBook [chloe, isaac, andy]

    -- toString [a, b]


    -- let
    --     {-
    --         Maybe.map : (a -> b) -> Maybe a -> Maybe b
    --         It takes a mapping function (a -> b), apply it on
    --         a value (Maybe a), then:
    --             if it is Nothing: return Nothing
    --             if it is Just a : apply (a -> b), returns Just b
    --
    --         This function is useful in situations where you want to
    --         change a Maybe value, but is not at an appropriate place
    --         to handle the case of Nothing. So you just leave Nothing
    --         as is, waiting for functions up the chain who are at a
    --         better position to handle it.
    --
    --         Maybe.withDefault : a -> Maybe a -> a
    --         It takes a default value and a Maybe value, if the Maybe
    --         value is Nothing, then use the default value, otherwise
    --         use the Maybe's inner value.
    --
    --         This function is used in situations where you know how to
    --         handle the case of Nothing.
    --
    --         Combining the two functions, we can do something interesting,
    --         such as showing a Maybe value to the output.
    --     -}
    --     resultAsString : Maybe a -> String
    --     resultAsString =
    --         Maybe.withDefault "nothing" << Maybe.map toString
    -- in
    --     -- resultAsString <| maximum <| []
    --     -- resultAsString <| maximum <| [ [] ]
    --     -- resultAsString <| maximum <| [ [1, 8.8, 3], [], [12.5], [-2, 11] ]
    --
    --     -- resultAsString <| maximum2 <| []
    --     -- resultAsString <| maximum2 <| [ [] ]
    --     resultAsString <| maximum2 <| [ [1, 8.8, 3], [], [12.5], [-2, 11] ]
