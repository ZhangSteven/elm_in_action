module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)


type alias Photo =
    { url : String }

type alias Model =
    { photos : List Photo, selectedUrl : String}

type alias Msg =
    { operation : String, data : String }


initialModel : Model
initialModel =
    { photos =
        [ {url = "1.jpeg"}
        , {url = "2.jpeg"}
        , {url = "3.jpeg"}
        ]
    , selectedUrl = "1.jpeg"
    }


-- create an Array of photos, for random access
photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


urlPrefix: String
urlPrefix =
    "http://elm-in-action.com/"


update : Msg -> Model -> Model
update msg model =
    case msg.operation of
        "SELECT_PHOTO" -> { model | selectedUrl = msg.data }
        "SURPRISE_ME"  -> { model | selectedUrl = "2.jpeg" }
        _ -> model


{-
    The view function is responsible for generating the HTML data,
    how that data is rendered, i.e., style, is included in the HTML file.
    For example, the class 'content', 'large', 'selected' (viewThumbnail)
    style definition is in index.html.
-}
view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [text "Photo Groove" ]
        , button
            [ onClick { operation = "SURPRISE_ME", data = "" } ]
            [ text "Surprise Me!" ]
        , div
            [id "thumbnails"]
            (List.map (viewThumbnail model.selectedUrl)
                model.photos
            )
        , img
            [ class "large"    -- show the selected image as a big one
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    {- The Html.classList function, builds a 'class' attribute using
        a list of tuples, with each tuple containing first the desired
        class name, and second a boolean for whether to include the
        class. -}

    img
        [ src (urlPrefix ++ thumbnail.url)
        , classList [("selected", selectedUrl == thumbnail.url)]
        , onClick {operation = "SELECT_PHOTO", data = thumbnail.url}
        ]
        []



main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
