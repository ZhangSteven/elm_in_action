{-
    Randomness in Elm

    To test it, compile as below and then open dice.html

    elm-make TestRondom.elm --output dice.js
-}
module TestRondom exposing(..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Random


{-
    In Elm, there is no counterpart to JavaScript's Math.random() function
    because functions in Elm are pure, i.e., they produce the same output
    given the same input.

    Therefore randomness is implemented using a "command", which describes
    an operation for the Elm runtime to perform. Unlike functions, running
    the same command multiple times can have different results.

    The dice example illustrates this flow:

    View --> send command
-}
type alias Model =
    { dice : String }

type Msg =
    Roll    -- for the 'roll' operation
    | Dice Int  -- for the 'dice' output

initialModel : Model
initialModel =
    { dice = "" }


{-
    Random.Generator: specify the type of value we want to randomly generate.
    For example, the Random.int function takes a lower bound and an upper
    bound, and returns a Random.Generator that generates a random Int between
    those bounds.
-}
rollDice : Random.Generator Int
rollDice =
    Random.int 1 6


{-
    To use command, we need to change the 'update' function's signature,
    so that it returns not only the new Model we want, but also the
    command we want the Elm runtime to execute.

    To generate a random number. We don't say "generate a random number
    right now," we say "I want a random number, so please generate one
    and send it to my update function wrapped in a Msg".
-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        {-
            Initiate a command to generate random number

            The Random.generate function returns a command that generates
            random values wrapped up in messages. The function takes 2
            parameters:

            1. A Random.Generator, which specify the type, range of the
                random number we want, like 'rollDice'.
            2. A function that takes the random value generated and returns
                a Msg from it, like 'Dice'.
        -}
        Roll -> (model, Random.generate Dice rollDice)

        -- Random number received in a Msg
        Dice result -> ({ model | dice = toString result }, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Dice Game" ]
        , p [] [ text ("dice : " ++ model.dice) ]
        , button
            [ onClick Roll ]
            [ text "Roll Dice" ]
        ]


{-
    To have 'update' return (Model, Cmd Msg) instead of just Model, we
    must use Html.program instead of Html.beginnerProgram.
-}
main : Program Never Model Msg
main =
    Html.program
        { init = (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }
