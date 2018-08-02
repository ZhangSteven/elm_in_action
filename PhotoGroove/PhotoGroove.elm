module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, classList, src, name, type_, title)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random
import Http
import Json.Decode exposing (Decoder, int, string, list)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }

type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String    -- use Maybe because the photo List can
                                    -- be empty
    , loadingError : Maybe String   -- there may be errors
    , choosenSize : ThumbnailSize
    }

type Msg =
    SelectByUrl String
    | SelectByIndex Int
    | SetSize ThumbnailSize
    | SurpriseMe
    | LoadPhotos (Result Http.Error (List Photo))

type ThumbnailSize
    = Small
    | Medium
    | Large



initialModel : Model
initialModel =
    { photos = []
    , selectedUrl = Nothing
    , loadingError = Nothing
    , choosenSize = Medium
    }


{-
    The initial command to send before the page is rendered.

    Http.send : (Result Error value -> msg) -> Http.Request value -> Cmd msg

    This function generates a command. The command will tell Elm runtime to
    send a Http request and upon receiving a response, convert that response
    to a message used by the "update" function.

    Http.get : String -> Decoder.Decoder value -> Http.Request value

    This function creates a Http request, if the request succeeds, the Decoder
    is used to turn the response string to data of type 'value'.
-}
initialCmd : Cmd Msg
initialCmd =
    let
        photoDecoder : Decoder Photo
        photoDecoder =
            decode Photo
                |> required "url" string
                |> required "size" int
                |> optional "title" string "(untitled)"
    in
        Http.send LoadPhotos
            <| Http.get "http://elm-in-action.com/photos/list.json"
            <| list photoDecoder


urlPrefix: String
urlPrefix =
    "http://elm-in-action.com/"


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectByUrl url -> ({ model | selectedUrl = Just url }, Cmd.none)
        SurpriseMe -> (model,
                        let
                            randomPhotoPicker : Random.Generator Int
                            randomPhotoPicker =
                                Random.int 0 (List.length model.photos - 1)
                        in
                            Random.generate SelectByIndex randomPhotoPicker
                      )
        SelectByIndex index -> ({model |
                                    selectedUrl =
                                        Maybe.map .url <| Array.get index
                                            <| Array.fromList model.photos
                                }
                                , Cmd.none
                               )
        SetSize size_ -> ({ model | choosenSize = size_ }, Cmd.none)

        LoadPhotos (Ok photos) ->
            ({ model | photos = photos
                , selectedUrl = Maybe.map .url <| List.head photos
             }
            , Cmd.none
            )

        LoadPhotos (Err _) ->
            ({ model |
                loadingError = Just "Error! (Try turning it off and on again?)"
             }
            , Cmd.none
            )


{-
    Create a helper function to add HTML node.

    Html.node function is used to create node in the virtual DOM. functions
    like div, img, button are all helper functions that call node under the
    cover. For example,

    node "button" [ class "large" ] [ text "Send" ]
          button  [ class "large" ] [ text "Send" ]

    are the same.

    You can think of button implemented as:

    button =
        node "button"
-}
paperSlider =
    node "paper-slider"


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
        , div [ class "filters" ]
            [ viewFilter "Hue" 0
            , viewFilter "Ripple" 0
            , viewFilter "Noise" 0
            ]
        , h3 [] [ text "Thumbnail Size: " ]
        , div
            [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div
            [ id "thumbnails", class (sizeToString model.choosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , viewLarge model.selectedUrl
        ]


{-
    Render the slider component
-}
viewFilter : String -> Int -> Html Msg
viewFilter name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Html.Attributes.max "11" ] []
        , label [] [ text <| toString magnitude ]
        ]


{-
    Render a thumbnail

    A thumbnail is registered with an onClick handler, which sends a message
    of type Msg

    A thumbnail is applied with class attribute "selected" if the selectedUrl
    is the same as its url. The style sheet is responsible for displaying
    special effect for the selected thumbnail.
-}
viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    {- The Html.classList function, builds a 'class' attribute using
        a list of tuples, with each tuple containing first the desired
        class name, and second a boolean for whether to include the
        class. -}

    img
        [ src (urlPrefix ++ thumbnail.url)
        , title <| thumbnail.title ++ " [" ++ toString thumbnail.size ++ " KB]"
        , classList [("selected", selectedUrl == Just thumbnail.url)]
        , onClick <| SelectByUrl thumbnail.url
        ]
        []


-- render the large image
viewLarge : Maybe String -> Html Msg
viewLarge selectedUrl =
    case selectedUrl of
        Nothing -> text ""  -- render nothing at all
        Just url ->
            img [ class "large", src <| urlPrefix ++ "large/" ++ url ] []


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


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing -> view model
        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [ text errorMessage ]
                ]



main =
    Html.program
        { init = (initialModel, initialCmd)
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
