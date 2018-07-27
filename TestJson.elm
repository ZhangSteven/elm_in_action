{-
    Test Jason decoder
-}
module TestJSON exposing (..)
import Json.Decode exposing (Decoder, decodeString, bool, float, int,
                            string, list, field, map2, nullable)

import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


{-
    Json.Decode.decodeString

    This function takes 2 parameters and return a
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
target = ["true", "false", "True", "haha"]
target2 = ["43", "-1", "0", "3.45", "hello"]
target3 = ["2.22", "-0.35", "18", "ok"]

-- Decode a JSON string with a Decoder, and convert the result to String
convert : Decoder a -> String -> String
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
target4 = "[1, 2.5, -0.88]"
target5 = """["hi", "yo"]"""
target6 = "[ [], [0], [1, 2] ]"


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
target7 = """{"x": 3.15, "y" : "hello"}"""


{-
    Json.Decode.mapX

    Compose everything together, create a record from a JSON object.

    Json.Decode.map2 takes a function and two field decoders to extract
    two field values and call the function to produce the final result.

    If the target JSON object has more than 2 fields, there are map3,
    map4, ... map8 functions available.

    If the JSON objects are more complex, then it's worth checking out
    NoRedInk/elm-decode-pipeline library.
-}

-- define both a record type and a function
type alias MyRecord = { x : Float, y : String }
target8 = """{"x": 3.15, "y" : "hello"}"""


{-
    Json.Decode.Pipeline.decode

    The decode function works similar to mapX function above, which
    takes a function and a list of field Decoders, then return a Decoder.

    The difference is:

    1. We can have as many fields as we like.
    2. The field decoders are 'required', 'optional', 'hardcoded'.

    The function comes in handy when we have lots of fields or we want to
    provide default values for certains fields.
-}
type alias User =
    { id : Int
    , email : Maybe String
    , name : String
    , percentExcited : Float
    }

userDecoder : Decoder User
userDecoder =
    decode User
        |> required "id" int
        |> required "email" (nullable string) -- 'null' decodes to Nothing
                                              -- but the field is required
        |> optional "name" string "(no name)" -- when name is 'null' or not
                                              -- there, use default value
        |> hardcoded 1.0

target9 = """{"id": 2357, "email": "abc@def.com", "name": "philip"}"""
target10 = """{"id": 1001, "email": null, "name": "zhangst"}"""
target11 = """{"id": 1001, "email": null}"""
target12 = """{"id": 1101, "name": "philip"}""" -- leads to error, because
                                                -- "email" field is missing


output : String
output =
    -- toString <| List.map (convert bool) target
    -- toString <| List.map (convert int) target2
    -- toString <| List.map (convert float) target3
    -- toString <| convert (list float) target4
    -- toString <| convert (list string) target5
    -- toString <| convert (list (list int)) target6
    -- toString <| convert (field "x" float) target7

    -- Compose everything together
    -- let
    --     recordDecoder = map2 MyRecord (field "x" float) (field "y" string)
    -- in
    --     toString <| convert recordDecoder target8

    -- Try pipeline decode
    -- toString <| convert userDecoder target9
    -- toString <| convert userDecoder target10
    -- toString <| convert userDecoder target11
    -- toString <| convert userDecoder target12 -- Error

    {-
        Compose everything together.

        The API is designed as composable.

        1. decodeString takes Decoder and String
        2. list take Decoder and returns a Decoder
        3. mapX takes a function and a list of field decoders and returns
            a decoder.
        4. decode takes a function and a list of 'required', 'optional'
            helper functions and returns a decoder.

        With the help of the above we can decode a JSON string representing
        a list of records.
    -}
    let
        target = "[ " ++ target9 ++ ", "
                    ++ target10 ++ ", "
                    ++ target11 ++ " ]"
    in
        toString <| convert (list userDecoder) target
