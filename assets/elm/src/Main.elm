module Main exposing (main)

import Html exposing (..)


main =
    div []
        [ text "Hello from Elm!"
        , button [ href "/logout" ] [ text "logout" ]
        ]
