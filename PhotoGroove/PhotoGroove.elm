{-
    This version sends HTTP request to a server to obtain the list
    of photos to display.
-}

module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random
import Http


type alias Photo =
    { url : String }

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
    | LoadPhotos (Result Http.Error String)

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


initialCmd : Cmd Msg
initialCmd =
    Http.send
        LoadPhotos
        (Http.getString "http://elm-in-action.com/photos/list")
-- initialCmd =
--     Task.perform handleLoadFailure handleLoadSuccess
--         <| Http.getString "http://elm-in-action.com/breakfast-burritos/list"


-- create an Array of photos, for random access
photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


urlPrefix: String
urlPrefix =
    "http://elm-in-action.com/"


-- Question: This is not a pure function, how to change it?
-- randomPhotoPicker : Random.Generator Int
-- randomPhotoPicker =
--     Random.int 0 (Array.length photoArray - 1)


-- Question: This is not a pure function, how to change it?
-- getPhotoUrl : Int -> Maybe String
-- getPhotoUrl index =
--     Maybe.map .url <| Array.get index photoArray


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
        {- The below code compiles but let's refactor the nested case statement
        LoadPhotos result ->
            case result of
                Ok responseStr ->
                    let
                        urls = String.split "," responseStr
                    in
                        ({ model | photos = List.map Photo urls }, Cmd.none)

                Err _ ->
                    (model, Cmd.none)   -- do nothing
        -}
        LoadPhotos (Ok responseStr) ->
            let
                urls = String.split "," responseStr
            in
                ({ model | photos = List.map Photo urls
                    , selectedUrl = List.head urls
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
        , viewLarge model.selectedUrl
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
        {-
            This works. But there is a better way to compare String to
            Maybe String

        , classList [("selected",
                        case selectedUrl of
                            Nothing -> False
                            Just url -> url == thumbnail.url
                    )]
        -}
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
