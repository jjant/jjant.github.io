module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src, class, href)


---- MODEL ----


type alias Model =
    {}


init =
    {}



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> Model
update _ model =
    model



---- VIEW ----


view _ =
    div [ class "main" ]
        [ img [ class "logo", src "haskell.png" ] []
        , div [ class "links" ] (List.map renderLink links)
        ]


renderLink : ( String, String ) -> Html a
renderLink ( site, url ) =
    a
        [ class "link", href url ]
        [ text site ]


links =
    [ ( "GitHub", "https://github.com/jjant" )
    , ( "Blog", "https://medium.com/@jjant" )
    , ( "Twitter", "https://twitter.com/_jjant" )
    , ( "LinkedIn", "https://www.linkedin.com/in/julianantonielli" )
    ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }
