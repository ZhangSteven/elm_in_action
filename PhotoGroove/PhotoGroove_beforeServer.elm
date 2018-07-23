{-
    This version uses fixed set of photos. Next we will load list of
    photos from a server.
-}

module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random


type alias Photo =
    { url : String }

type alias Model =
    { photos : List Photo, selectedUrl : String, choosenSize : ThumbnailSize }

type Msg =
    SelectByUrl String
    | SelectByIndex Int
    | SetSize ThumbnailSize
    | SurpriseMe

type ThumbnailSize
    = Small
    | Medium
    | Large


initialModel : Model
initialModel =
    { photos =
        [ {url = "1.jpeg"}
        , {url = "2.jpeg"}
        , {url = "3.jpeg"}
        ]
    , selectedUrl = "1.jpeg"
    , choosenSize = Medium
    }


-- create an Array of photos, for random access
photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


urlPrefix: String
urlPrefix =
    "http://elm-in-action.com/"


-- Question: This is not a pure function, how to change it?
randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)


-- Question: This is not a pure function, how to change it?
getPhotoUrl : Int -> String
getPhotoUrl index =
    let
        photo = Maybe.withDefault { url = "" } <| Array.get index photoArray
    in
        photo.url


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectByUrl url -> ({ model | selectedUrl = url }, Cmd.none)
        SurpriseMe -> (model, Random.generate SelectByIndex randomPhotoPicker)
        SelectByIndex index -> ({model | selectedUrl = getPhotoUrl index},
                                    Cmd.none
                               )
        SetSize size_ -> ({ model | choosenSize = size_ }, Cmd.none)


{-
    The view function is responsible for generating the HTML data,
    how that data is rendered, i.e., style, is included in the HTML file.
    For example, the class 'content', 'large', 'selected' (viewThumbnail)
    style definition is in index.html.

    The function view takes a Model and returns Html, but we cannot put
    its type as Model -> Html, because Html, like List and Array, has a
    type vairiable, i.e, Html a.

    Html's type variable reflects the type of message it sends to update
    in response to events from handlers like onClick.
-}
view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [text "Photo Groove" ]
        , button
            [ onClick SurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size: " ]
        , div
            [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div
            [ id "thumbnails", class (sizeToString model.choosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"    -- show the selected image as a big one
            , src <| urlPrefix ++ "large/" ++ model.selectedUrl
            ]
            []
        ]


{-
    Render a thumbnail

    A thumbnail is registered with an onClick handler, which sends a message
    of type Msg

    A thumbnail is applied with class attribute "selected" if the selectedUrl
    is the same as its url. The style sheet is responsible for displaying
    special effect for the selected thumbnail.
-}
viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    {- The Html.classList function, builds a 'class' attribute using
        a list of tuples, with each tuple containing first the desired
        class name, and second a boolean for whether to include the
        class. -}

    img
        [ src (urlPrefix ++ thumbnail.url)
        , classList [("selected", selectedUrl == thumbnail.url)]
        , onClick <| SelectByUrl thumbnail.url
        ]
        []


-- render a thumbnail size radio button
viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size_ =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick <| SetSize size_
            ]
            []
        , text <| sizeToString size_
        ]


sizeToString : ThumbnailSize -> String
sizeToString size_ =
    case size_ of
        Small -> "small"
        Medium -> "medium"
        Large -> "large"



main =
    Html.program
        { init = (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }
