{-
    Test Jason decoder
-}
module TestJson exposing(..)
import Json.Decode exposing(decodeString, bool, float, int,
                            string, list, field, map2)


{-
    The decodeString function.

    The Json.Decode.decodeString function takes 2 parameters and return a
    Result

    1. A decoder, which translates a JSON string into Elm data
        types, like Bool, Int or Float value.
    2. A target string to be decoded.

    The only primitive decoders are bool, int, float, string and null.

    In elm-repl,

    > decodeString
    <function> : Json.Decode.Decoder a -> String -> Result.Result String a

    > bool
    <decoder> : Json.Decode.Decoder Bool

    > float
    <decoder> : Json.Decode.Decoder Float


    Result is a union type defined as:

    Result =
        Ok resultType
        | Err errorType

    where resultType and errorType are type variables. Result is used to
    deliver results by many modules, for example in the PhotoGroove app,
    the Http module returns results as Result Ok String Err Http.Error.

    In the case of decodeString function, the error type is always String,
    but the response type depends on the decoder used. For example, if a
    Boolean value decoder (bool) is used, the response type is Bool. Then
    the Result type would be: Result Ok Bool Err String.
-}
-- target = ["true", "false", "True", "haha"]
-- target = ["43", "-1", "0", "3.45", "hello"]
-- target = ["2.22", "-0.35", "18", "ok"]

convert decoder string =
    let
        result = decodeString decoder string
    in
        case result of
            Ok value -> toString value  -- value can be any type
            Err error -> error  -- error is always of type String


{-
    Json.Decode.list

    What if the JSON string is not an independant String like "true",
    "25" or "-.73", but instead comes in as a list format "[1, 2, 3]"?

    In this case, we need the list function, which takes a decoder (e.g. int),
    and returns a decoder that deocdes string representing list of that
    type.

    In elm-repl,

    > list
    <function> : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)

    > list int
    <decoder> : Json.Decode.Decoder (List Int)
-}
-- target = "[1, 2.5, -0.88]"
-- target = """["hi", "yo"]"""
-- target = "[ [], [0], [1, 2] ]"


{-
    Json.Decode.field

    What if the JSON string represents a JSON object, e.g.
    "{x:3, y:"hello"}", and we need to extract field values from it?

    In this case, we can use the field function to extract the value
    associated with a field name. The function takes two parameters
    and returns a decoder. We can use pass that decoder to decodeString
    function to parse a JSON string. The parameters needed are:

    1. The field name, a String
    2. The decoder for the value associated with that field

    In elm-repl,

    > field "x" int
    <decoder> : Json.Decode.Decoder Int
-}
-- target = """{"x": 3.15, "y" : "hello"}"""


{-
    Json.Decode.mapx

    Compose everything together, create a record from a JSON object.

    Json.Decode.map2 takes a function and two field decoders to extract
    two field values and call the function to produce the final result.

    If the target JSON object has more than 2 fields, there are map2,
    map3, ... map8 functions available.

    If the JSON objects are more complex, then it's worth checking out
    NoRedInk/elm-decode-pipeline library.
-}

-- define both a record type and a function
type alias MyRecord = { x : Float, y : String }
target = """{"x": 3.15, "y" : "hello"}"""




output : String
output =
    -- toString <| List.map (convert bool) target
    -- toString <| List.map (convert int) target
    -- toString <| List.map (convert float) target
    -- toString <| convert (list float) target
    -- toString <| convert (list string) target
    -- toString <| convert (list (list int)) target
    -- toString <| convert (field "x" float) target

    -- Compose everything together
    let
        recordDecoder = map2 MyRecord (field "x" float) (field "y" string)
    in
        toString <| convert recordDecoder target
