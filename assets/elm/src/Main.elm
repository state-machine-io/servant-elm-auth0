module Main exposing (main)

import Html as Html
import Html.Attributes as HtmlAttributes


main =
    Html.div []
        [ Html.text "Hello from Elm!"
        , Html.form [ HtmlAttributes.action "/logout" ] [ Html.button [ HtmlAttributes.type_ "submit" ] [ Html.text "logout" ] ]
        ]
