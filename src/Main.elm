module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src, class, href, height, style, width, alt)


---- MODEL ----


type alias Model =
    ()


init : () -> ( (), Cmd Msg )
init _ =
    ( ()
    , Cmd.none
    )



-- Update


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( ()
    , Cmd.none
    )



---- VIEW ----


view _ =
    div [ class "main" ]
        [ img [ class "logo", src "haskell.png", alt "Logo" ] []
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


subscriptions : Model -> Sub.Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , subscriptions = subscriptions
        , update = update
        }
