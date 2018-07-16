module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


initialModel =
    {photos =
        [{url = "1.jpeg"}
        ,{url = "2.jpeg"}
        ,{url = "3.jpeg"}
        ]
    ,selectedUrl = "1.jpeg"
    }


urlPrefix =
    "http://elm-in-action.com/"


update msg model =
    if msg.operation == "SELECT_PHOTO" then
        { model | selectedUrl = msg.data }
    else
        model


view model =
    div [class "content"]
        [h1 [] [text "Photo Groove"]
        , div [id "thumbnails"]
            (List.map (viewThumbnail model.selectedUrl)
                model.photos
            )
        , img [class "large"    -- show the selected image as a big one
                , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail selectedUrl thumbnail =
    {- The Html.classList function, builds a 'class' attribute using
        a list of tuples, with each tuple containing first the desired
        class name, and second a boolean for whether to include the
        class. -}

    img [ src (urlPrefix ++ thumbnail.url)
        , classList [("selected", selectedUrl == thumbnail.url)]
        , onClick {operation = "SELECT_PHOTO", data = thumbnail.url}
        ]
        []

    {- The above code is same as below

    if selectedUrl == thumbnail then
        img [src (urlPrefix ++ thumbnail.url)
            ,class "selected"
            ]
            []
    else
        img [src (urlPrefix ++ thumbnail.url)] []
    -}


main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
