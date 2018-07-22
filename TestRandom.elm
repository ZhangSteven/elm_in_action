{-
    Randomness in Elm
-}
module TestRondom exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


{-
    In Elm, there is no counterpart to JavaScript's Math.random() function
    because functions in Elm are pure, i.e., they produce the same output
    given the same input.

    Therefore randomness is obtained through "command", instead of calling
    a function, we send a message to Elm runtime, specify what kind of
    random number we want (float or int, range), in what way we want to
    receive them.

    Here is how.
-}
type alias Model =
    { dice : String }

type Msg =
    Roll    -- for the 'roll' operation
    | Dice Int  -- for the 'dice' output

initialModel : Model
initialModel =
    { dice = "" }



rollDice : Random.Generator Int
rollDice =
    Random.int 1 6


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Roll -> (model, Random.generate Dice rollDice)
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


main =
    Html.program
        { init = (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }
