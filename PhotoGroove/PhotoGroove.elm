port module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, classList, src, name, type_, title)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random
import Http
import Json.Decode exposing (Decoder, int, string, list, at)
import Json.Decode.Pipeline exposing (decode, required, optional)
-- import Ports exposing (..)


{-
    Declare functions to send and receive messages to and from Elm runtime.
-}
port setFilters : FilterOptions -> Cmd msg
port statusChanges : (String -> msg) -> Sub msg

type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }

type alias Photo =
    { url : String
    , size : Int
    , title : String
    }

type alias Model =
    { photos : List Photo
    , status : String
    , selectedUrl : Maybe String    -- use Maybe because the photo List can
                                    -- be empty
    , loadingError : Maybe String   -- there may be errors
    , choosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }

type Msg =
    SelectByUrl String
    | SetStatus String
    | SetHue Int
    | SetRipple Int
    | SetNoise Int
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
    , status = ""
    , selectedUrl = Nothing
    , loadingError = Nothing
    , choosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
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
        -- SelectByUrl url -> ({ model | selectedUrl = Just url }, Cmd.none)
        SelectByUrl url ->
            applyFilters { model | selectedUrl = Just url }

        SetStatus status ->
            ({ model | status = status }, Cmd.none)

        SurpriseMe -> (model,
                        let
                            randomPhotoPicker : Random.Generator Int
                            randomPhotoPicker =
                                Random.int 0 (List.length model.photos - 1)
                        in
                            Random.generate SelectByIndex randomPhotoPicker
                      )

        SelectByIndex index ->
            let
                newModel ={ model | selectedUrl =
                                Maybe.map .url <| Array.get index
                                    <| Array.fromList model.photos
                          }
            in
                applyFilters newModel

        SetSize size_ -> ({ model | choosenSize = size_ }, Cmd.none)

        LoadPhotos (Ok photos) ->
            -- ({ model | photos = photos
            --     , selectedUrl = Maybe.map .url <| List.head photos
            --  }
            -- , Cmd.none
            -- )
            applyFilters { model
                            | photos = photos
                            , selectedUrl = Maybe.map .url <| List.head photos
                         }

        LoadPhotos (Err _) ->
            ({ model |
                loadingError = Just "Error! (Try turning it off and on again?)"
             }
            , Cmd.none
            )

        SetHue hue ->
            applyFilters { model | hue = hue }

        SetRipple ripple ->
            applyFilters { model | ripple = ripple }

        SetNoise noise ->
            applyFilters { model | noise = noise }


{-
    Either selecting a image by hand or pressing the "Surprise me" button will
    select a image, we want to pass that image url to the filters.
-}
applyFilters : Model -> (Model, Cmd Msg)
applyFilters model =
    case model.selectedUrl of
        Just selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl

                cmd =
                    setFilters { url = url, filters = filters }
            in
                (model, cmd)

        Nothing ->
            (model, Cmd.none)


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
    Custom event handler.

    While Html.Events provides event handlers for the most basic events,
    e.g., onClick, it does not cover all possible events. However, it does
    expose a few low level functions we can use to implement our own custom
    event handler.

    A custom event handler is composed of four parts:

    1. The Html.events.on function to create the handler.
    2. The name of a DOM event.
    3. A Msg constructor, creating messages to the update function.
    4. A JSON decoder, so that we can pull a value out of a JavaScript object
        and into Elm.
-}
onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
    -- let
    --     {-
    --         toMsg: Int -> Msg
    --         convert the decoded value (int) into a value of type Msg.
    --
    --         targetImmediateValue : decode value from JavaScript to an Int
    --                                value.
    --
    --         Json.Decode.map : use a function (a -> b) to transform Decoder a
    --                           to Decoder b
    --
    --         msgDecoder : the decoder that transfrom value from JavaScript
    --                      to a value of type Msg
    --     -}
    --     targetImmediateValue : Decoder Int
    --     targetImmediateValue =
    --         at [ "target", "immediateValue" ] int
    --
    --     msgDecoder : Decoder msg
    --     msgDecoder =
    --         Json.Decode.map toMsg targetImmediateValue
    --
    -- in
    --     Html.Events.on "immediate-value-changed" msgDecoder

    {-
        The above code works
    -}
    Html.Events.on "immediate-value-changed" <| Json.Decode.map toMsg
        <| at [ "target", "immediateValue" ] int


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
        , div [ class "status" ] [ text model.status ]
        , div [ class "filters" ]
            [ viewFilter "Hue" SetHue model.hue
            , viewFilter "Ripple" SetRipple model.ripple
            , viewFilter "Noise" SetNoise model.noise
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
viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name toMsg magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Html.Attributes.max "11", onImmediateValueChange toMsg ] []
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
            -- img [ class "large", src <| urlPrefix ++ "large/" ++ url ] []
            canvas [ id "main-canvas", class "large" ] []


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
    -- Html.program
    --     { init = (initialModel, initialCmd)
    --     , view = viewOrError
    --     , update = update
    --     , subscriptions = (\_ -> statusChanges SetStatus)
    --     }

    Html.programWithFlags
        { init = init
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> statusChanges SetStatus)
        }


init : Float -> (Model, Cmd Msg)
init flags =
    let
        status =
            "Initializing Pasta v" ++ toString flags
    in
        ({ initialModel | status = status }, initialCmd)
